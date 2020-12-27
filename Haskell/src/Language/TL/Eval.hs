-- If we got to this point, we know typechecking succeeded, so we can use incomplete patterns
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module Language.TL.Eval(Val(..), ProgState(..), traverseSteps_, finalState, showOut, mkGCStats, mkProgState) where

import Numeric(showHex)
import Data.List(intercalate)
import Data.Functor(($>))
import Control.Monad(when)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Data.Set(Set)
import qualified Data.Set as S

import Control.Monad.Reader(local, asks, ask, MonadReader, runReaderT)
import Control.Monad.Except(throwError, MonadError)
import Data.Bifunctor(Bifunctor(first))
import Control.Monad.State (runStateT, modify, get, MonadState)

import Language.TL.Syntax

type Addr = Int

data Val
  = VInt Integer
  | VBool Bool
  | VStr String
  | forall f. VRec (Field f) (Map f Val)
  | VFun SymValTable Ident (Expr 'R)
  | VRef Addr

showH :: (Integral a, Show a) => a -> String
showH a = "0x" ++ showHex a ""

instance Show Val where
  show (VInt i) = show i
  show (VBool b) = show b
  show (VStr s) = show s
  show (VRec f m) = showFields False f " = " m
  show VFun{} = "<λ>"
  show (VRef a) = showH a

instance Ord Val where
  compare (VInt a) (VInt b) = compare a b
  compare (VBool a) (VBool b) = compare a b
  compare (VStr a) (VStr b) = compare a b
  compare (VRec FRec a) (VRec FRec b) = compare a b
  compare (VRec FTup a) (VRec FTup b) = compare a b
  compare _ _ = error "Opaque values in comparison. Did you run the typechecker?"

instance Eq Val where
  (==) = ((== EQ) .) . compare

defaultVal :: Type -> Val
defaultVal TInt = VInt 0
defaultVal TBool = VBool False
defaultVal TStr = VStr ""
defaultVal (TRec f m) = VRec f (defaultVal <$> m)
defaultVal _ = error "Opaque type in defaultVal. Did you run the typechecker?"

valType :: Val -> Type
valType (VInt _) = TInt
valType (VBool _) = TBool
valType (VStr _) = TStr
valType (VRec f m) = TRec f (valType <$> m)
valType _ = error "Opaque value in valType. Did you run the typechecker?"

type SymValTable = Map Ident Val
type ToDo = [Stmt]
type Out = [Val]
type FileTable = Map String [Val]
type Heap = Map Addr Val

data GCStats =
  GCStats
  { allocsSinceGC :: Int
  , gcThreshold :: Int
  , crrHeapSize :: Int
  , maxHeapSize :: Int
  }

instance Show GCStats where
  show GCStats{..} =
    "{ allocs = "
    ++ show allocsSinceGC ++ " / " ++ show gcThreshold
    ++ ", heapSize = " ++ show crrHeapSize ++ " / " ++ show maxHeapSize
    ++ " }"

mkGCStats :: Int -> Int -> GCStats
mkGCStats gcThreshold maxHeapSize =
  GCStats
  { allocsSinceGC = 0
  , gcThreshold
  , crrHeapSize = 0
  , maxHeapSize
  }

data ProgState =
  ProgState
  { fs :: FileTable
  , open :: FileTable
  , toDo :: ToDo
  , sym :: SymValTable
  , gcStats :: GCStats
  , heap :: Heap
  , out :: Out
  }

showL :: (a -> String) -> [a] -> String
showL fmt l = "[" ++ intercalate ", " (fmt <$> l) ++ "]"

instance Show ProgState where
  show ProgState{..} =
    unlines
      [ "fs = " ++ showM show (showL show) fs
      , "open = " ++ showM show (showL show) open
      , "toDo = " ++ showL show toDo
      , "sym = " ++ showM id show sym
      , "gcStats = " ++ show gcStats
      , "heap = " ++ showM showH show heap
      , "out = " ++ showL show (reverse out)
      ]
    where
      showM fmt fmtL m = withParens "{ " " }" (showVar fmt fmtL <$> M.toList m)
      showVar fmt fmtL (ident, var) = fmt ident ++ " = " ++ fmtL var

showOut :: ProgState -> String
showOut = unlines . reverse . (show <$>) . out

mkProgState :: FileTable -> GCStats -> Stmt -> ProgState
mkProgState fs gcStats stmt =
  ProgState
  { fs
  , open = M.empty
  , toDo = [stmt]
  , sym = M.empty
  , out = []
  , gcStats
  , heap = M.empty
  }

type EvalEnv = (SymValTable, Heap)

evalEnv :: ProgState -> EvalEnv
evalEnv ProgState{sym, heap} = (sym, heap)

data EvalError
  = DivisionByZero
  | FileDoesNotExist String
  | FileAlreadyOpened String
  | FileAlreadyClosed String
  | FileNotOpened String
  | ReadDifferentType String Type Type
  | ReachedEOF String
  | OutOfMemory Int

instance Show EvalError where
  show ee = "Eval error: " ++ go ee ++ "."
    where
      go DivisionByZero = "An attempt was made to divide by zero"
      go (FileDoesNotExist f) = "The file " ++ show f ++ " does not exist in the filesystem"
      go (FileAlreadyOpened f) = "The file " ++ show f ++ " has already been opened"
      go (FileAlreadyClosed f) = "The file " ++ show f ++ " has been closed"
      go (FileNotOpened f) = "The file " ++ show f ++ " has not been opened"

      go (ReadDifferentType f found expected) =
        "In file " ++ show f
        ++ ", found a value of type " ++ show found
        ++ ", but expected a value of type " ++ show expected

      go (ReachedEOF f) = "There are no more values to read in file " ++ show f
      go (OutOfMemory size) = "Out of memory (heap size: " ++ show size ++ ")"

type Eval a = Either EvalError a

evalExpr :: (MonadReader EvalEnv m, MonadError EvalError m) => Expr a -> m Val
evalExpr (Default t) = pure $ defaultVal t
evalExpr (IntLit i) = pure $ VInt i
evalExpr (BoolLit b) = pure $ VBool b
evalExpr (StrLit s) = pure $ VStr s
evalExpr (Var ident) = asks $ (M.! ident) . fst

evalExpr (Arith a op b) = do
  a' <- evalExpr a
  b' <- evalExpr b
  case (b', op) of
    (VStr vb, Add) ->
      case a' of
        VStr va -> pure $ VStr $ va ++ vb
    (VInt 0, Divide) -> throwError DivisionByZero
    (VInt 0, Remainder) -> throwError DivisionByZero
    (VInt vb, _) ->
      case a' of
        VInt va -> pure $ VInt $ arithOp op va vb

evalExpr (Logic a op b) = do
  a' <- evalExpr a
  case (a', op) of
    (VBool True, Or) -> pure a'
    (VBool False, And) -> pure a'
    (VBool va, _) -> do
      b' <- evalExpr b
      case b' of
        VBool vb -> pure $ VBool $ logicOp op va vb

evalExpr (Comp a op b) = do
  a' <- evalExpr a
  b' <- evalExpr b
  pure $ VBool $ compOp op a' b'

evalExpr (RecLit f m) = VRec f <$> traverse evalExpr m

evalExpr (RecMember lhs f i) = do
  v <- evalExpr lhs
  case (v, f) of
    (VRec FRec m, FRec) -> pure $ m M.! i
    (VRec FTup m, FTup) -> pure $ m M.! i

evalExpr (RecWith lhs f us) = do
  v <- evalExpr lhs
  vals <- traverse evalExpr us
  case (v, f) of
    (VRec FRec fs, FRec) ->
      pure $ VRec FRec $ M.foldlWithKey' (\m k v' -> M.insert k v' m) fs vals
    (VRec FTup fs, FTup) ->
      pure $ VRec FTup $ M.foldlWithKey' (\m k v' -> M.insert k v' m) fs vals

evalExpr (RecUnion a b) = do
  ra <- evalExpr a
  rb <- evalExpr b
  case (ra, rb) of
    (VRec FRec a', VRec FRec b') -> pure $ VRec FRec $ M.union a' b'

evalExpr (Lam i _ e) = do
  (sym, _) <- ask
  pure $ VFun sym i e

evalExpr (App f a) = do
  vf <- evalExpr f
  va <- evalExpr a
  case vf of
    VFun sym' i e -> local (first $ const $ M.insert i va sym') (evalExpr e)

evalExpr (Deref e) = do
  v <- evalExpr e
  case v of
    VRef addr -> asks $ (M.! addr) . snd

evalExpr' :: (MonadState ProgState m, MonadError EvalError m) => Expr a -> m Val
evalExpr' e = runReaderT (evalExpr e) . evalEnv =<< get

evalStmt :: (MonadState ProgState m, MonadError EvalError m) => Stmt -> m ()
evalStmt Nop = pure ()
evalStmt (Decl _ _) = pure ()

evalStmt (Assign (Var ident) expr) = do
  v <- evalExpr' expr
  modify \p -> p { sym = M.insert ident v $ sym p }

evalStmt (Assign (RecMember lhs f i) expr) =
  evalStmt (Assign lhs (RecWith lhs f $ M.singleton i expr))

evalStmt (DeclAssign ident (Just type') expr) =
  modify \p -> p { toDo = Decl ident type' : Assign (Var ident) expr : toDo p }

evalStmt (DeclAssign ident Nothing expr) =
  modify \p -> p { toDo = Assign (Var ident) expr : toDo p }

evalStmt (Print expr) = do
  v <- evalExpr' expr
  modify \p -> p { out = v : out p }

evalStmt (If cond then' else') = do
  c' <- evalExpr' cond
  case c' of
    VBool c -> modify \p -> p { toDo = (if c then then' else else') : toDo p }

evalStmt w@(While cond body) = do
  c' <- evalExpr' cond
  case c' of
    VBool c -> modify \p -> p { toDo = if c then body : w : toDo p else toDo p }

evalStmt (Compound a b) =
  modify \p -> p { toDo = a : b : toDo p }

evalStmt (Open expr) = do
  ProgState{..} <- get
  vf <- evalExpr' expr
  case vf of
    VStr f ->
      case M.lookup f fs of
        Nothing -> throwError $ FileDoesNotExist f
        Just content ->
          case M.lookup f open of
            Just _ -> throwError $ FileAlreadyOpened f
            Nothing -> modify \p -> p { open = M.insert f content open }

evalStmt (Read ident t expr) = do
  ProgState{..} <- get
  vf <- evalExpr' expr
  case vf of
    VStr f ->
      case M.lookup f open of
        Nothing -> throwError $ FileNotOpened f
        Just [] -> throwError $ ReachedEOF f
        Just (c : cs) ->
          let tc = valType c
          in if tc /= t
            then throwError $ ReadDifferentType f tc t
            else modify \p -> p { sym = M.insert ident c sym, open = M.insert f cs open }

evalStmt (Close expr) = do
  ProgState{..} <- get
  vf <- evalExpr' expr
  case vf of
    VStr f ->
      case M.lookup f open of
        Nothing -> throwError $ FileAlreadyClosed f
        Just _ -> modify \p -> p { open = M.delete f open }

evalStmt (New i e) = do
  ProgState { gcStats = GCStats{..}, .. } <- get

  when (crrHeapSize == maxHeapSize)
    $ throwError $ OutOfMemory maxHeapSize

  v <- evalExpr' e
  let sym' = M.insert i (VRef crrHeapSize) sym
  let heap' = M.insert crrHeapSize v heap

  -- This looks evil, it actually parses as `runGC $ p { .. }` :P
  modify \p -> runGC p { sym = sym', heap = heap' }

evalStmt (WriteAt lhs rhs) = do
  vl <- evalExpr' lhs
  vr <- evalExpr' rhs
  case vl of
    VRef addr -> modify \p -> p { heap = M.insert addr vr $ heap p }

compactKeys :: [Addr] -> (Addr -> Addr)
compactKeys keys = (M.!) $ M.fromList $ zip keys [0..]

runGC :: ProgState -> ProgState
runGC ProgState { gcStats = gcStats@GCStats { .. }, .. } =
  if allocsSinceGC < gcThreshold
    then ProgState { gcStats = gcStats { allocsSinceGC = succ allocsSinceGC, crrHeapSize = succ crrHeapSize }, .. }
    else
      let
        heap' = M.restrictKeys heap (getInnerAddrs heap sym)
        f = compactKeys $ M.keys heap'
        heapCompacted = mapInnerAddrsScope f heap'
        symCompacted = mapInnerAddrsScope f sym
      in
        ProgState
        { gcStats = gcStats { allocsSinceGC = 0, crrHeapSize = M.size heapCompacted }
        , sym = symCompacted
        , heap = M.mapKeys f heapCompacted
        , ..
        }

getInnerAddrsVal :: Val -> Set Addr
getInnerAddrsVal (VRef a) = S.singleton a
getInnerAddrsVal (VRec _ m) = getInnerAddrsScope m
getInnerAddrsVal (VFun sym _ _) = getInnerAddrsScope sym
getInnerAddrsVal _ = S.empty

getInnerAddrsScope :: Foldable f => f Val -> Set Addr
getInnerAddrsScope = foldMap getInnerAddrsVal

getInnerAddrsDerefed :: Heap -> Set Addr -> Set Addr
getInnerAddrsDerefed heap addrs = getInnerAddrsScope $ (heap M.!) <$> S.toList addrs

getInnerAddrsAll :: Heap -> Set Addr -> Set Addr -> Set Addr
getInnerAddrsAll heap acc set
  | S.null set = acc
  | otherwise = getInnerAddrsAll heap newAcc (getInnerAddrsDerefed heap set `S.difference` newAcc)
    where
      newAcc = acc <> set

getInnerAddrs :: Foldable f => Heap -> f Val -> Set Addr
getInnerAddrs heap = getInnerAddrsAll heap S.empty . getInnerAddrsScope

mapInnerAddrsVal :: (Addr -> Addr) -> Val -> Val
mapInnerAddrsVal f (VRef a) = VRef $ f a
mapInnerAddrsVal f (VRec f' m) = VRec f' $ mapInnerAddrsScope f m
mapInnerAddrsVal f (VFun sym i e) = VFun (mapInnerAddrsScope f sym) i e
mapInnerAddrsVal _ v = v

mapInnerAddrsScope :: Functor f => (Addr -> Addr) -> f Val -> f Val
mapInnerAddrsScope f = (mapInnerAddrsVal f <$>)

smallStep :: (MonadState ProgState m, MonadError EvalError m) => m Bool
smallStep = go =<< get
  where
    go ProgState { toDo = [] } = pure True
    go ProgState { toDo = stmt : toDo } = do
      modify \p -> p { toDo }
      evalStmt stmt
      pure False

runSmallStep :: ProgState -> Maybe (Either EvalError ProgState)
runSmallStep p = do
  case runStateT smallStep p of
      Left e -> Just $ Left e
      Right (done, p') -> if done then Nothing else Just $ Right p'

traverseSteps_ :: Applicative f => (String -> f a) -> ProgState -> f ()
traverseSteps_ f state =
  f (show state) *>
    case runSmallStep state of
      Nothing -> pure ()
      Just (Left e) -> f (show e) $> ()
      Just (Right state') -> traverseSteps_ f state'

finalState' :: ProgState -> Eval ProgState
finalState' s =
  case runSmallStep s of
    Nothing -> pure s
    Just s' -> s' >>= finalState'

finalState :: ProgState -> TLI ProgState
finalState = toTLI . finalState'
