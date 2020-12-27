-- If we got to this point, we know typechecking succeeded, so we can use incomplete patterns
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module Language.TL.Eval(Val(..), EvalState(..), mkGCStats, mkEvalState, eval) where

import Numeric(showHex)
import Control.Monad(when)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Data.Set(Set)
import qualified Data.Set as S

import Control.Monad.Except(runExceptT, ExceptT, throwError, MonadError)
import Control.Monad.State (MonadState, gets, runStateT, modify, get)

import Language.TL.Syntax
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State.Lazy (StateT)

type Addr = Int

data Val
  = VInt Integer
  | VBool Bool
  | VStr String
  | forall f. VRec (Field f) (Map f Val)
  | VFun Env Ident Expr
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

unit :: Val
unit = VRec FTup M.empty

valType :: Val -> Type
valType (VInt _) = TInt
valType (VBool _) = TBool
valType (VStr _) = TStr
valType (VRec f m) = TRec f (valType <$> m)
valType _ = error "Opaque value in valType. Did you run the typechecker?"

type Env = Map Ident Val
type Files = Map String [Val]
type Heap = Map Addr Val

data GCStats =
  GCStats
  { allocsSinceGC :: Int
  , gcThreshold :: Int
  , crrHeapSize :: Int
  , maxHeapSize :: Int
  }
  deriving stock Show

mkGCStats :: Int -> Int -> GCStats
mkGCStats gcThreshold maxHeapSize =
  GCStats
  { allocsSinceGC = 0
  , gcThreshold
  , crrHeapSize = 0
  , maxHeapSize
  }

data EvalState =
  EvalState
  { fs :: Files
  , open :: Files
  , env :: Env
  , gcStats :: GCStats
  , heap :: Heap
  }
  deriving stock Show

mkEvalState :: Files -> GCStats -> EvalState
mkEvalState fs gcStats =
  EvalState
  { fs
  , open = M.empty
  , env = M.empty
  , gcStats
  , heap = M.empty
  }

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

withEnv :: MonadState EvalState m => Env -> m a -> m a
withEnv newEnv m = do
  oldEnv <- gets env
  modify \p -> p { env = newEnv }
  a <- m
  modify \p -> p { env = oldEnv }
  pure a

evalExpr :: (MonadState EvalState m, MonadError EvalError m, MonadIO m) => Expr -> m Val
evalExpr (IntLit i) = pure $ VInt i
evalExpr (BoolLit b) = pure $ VBool b
evalExpr (StrLit s) = pure $ VStr s
evalExpr (Var ident) = gets $ (M.! ident) . env

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

evalExpr (RecUnion a b) = do
  ra <- evalExpr a
  rb <- evalExpr b
  case (ra, rb) of
    (VRec FRec a', VRec FRec b') -> pure $ VRec FRec $ M.union b' a'

evalExpr (Lam i _ e) = gets $ (\env -> VFun env i e) . env

evalExpr (App f a) = do
  vf <- evalExpr f
  va <- evalExpr a
  case vf of
    VFun env' i e -> withEnv (M.insert i va env') (evalExpr e)

evalExpr (Deref e) = do
  v <- evalExpr e
  case v of
    VRef addr -> gets $ (M.! addr) . heap

evalExpr (Print expr) = do
  v <- evalExpr expr
  liftIO $ print v
  pure unit

evalExpr (If cond then' else') = do
  c' <- evalExpr cond
  case c' of
    VBool c -> do
      env <- gets env
      withEnv env (evalExpr if c then then' else else')

evalExpr (Open expr) = do
  EvalState{..} <- get
  vf <- evalExpr expr
  case vf of
    VStr f ->
      case M.lookup f fs of
        Nothing -> throwError $ FileDoesNotExist f
        Just content ->
          case M.lookup f open of
            Just _ -> throwError $ FileAlreadyOpened f
            Nothing -> unit <$ modify \p -> p { open = M.insert f content open }

evalExpr (Read t expr) = do
  EvalState{..} <- get
  vf <- evalExpr expr
  case vf of
    VStr f ->
      case M.lookup f open of
        Nothing -> throwError $ FileNotOpened f
        Just [] -> throwError $ ReachedEOF f
        Just (c : cs) ->
          let tc = valType c
          in if tc /= t
            then throwError $ ReadDifferentType f tc t
            else c <$ modify \p -> p { open = M.insert f cs open }

evalExpr (Close expr) = do
  EvalState{..} <- get
  vf <- evalExpr expr
  case vf of
    VStr f ->
      case M.lookup f open of
        Nothing -> throwError $ FileAlreadyClosed f
        Just _ -> unit <$ modify \p -> p { open = M.delete f open }

evalExpr (New e) = do
  modify runGC
  EvalState { gcStats = GCStats{..}, .. } <- get

  when (crrHeapSize == maxHeapSize) $
    throwError $ OutOfMemory maxHeapSize

  v <- evalExpr e

  modify \p ->
    p
    { heap = M.insert crrHeapSize v heap
    , gcStats =
        GCStats
        { crrHeapSize = succ crrHeapSize
        , allocsSinceGC = succ allocsSinceGC
        , ..
        }
    }

  pure $ VRef crrHeapSize

evalExpr (WriteAt lhs rhs) = do
  vl <- evalExpr lhs
  vr <- evalExpr rhs
  case vl of
    VRef addr -> unit <$ modify \p -> p { heap = M.insert addr vr $ heap p }

evalExpr (Let i _ v e) = do
  v' <- evalExpr v
  modify \p -> p { env = M.insert i v' (env p) }
  evalExpr e

evalExpr (Seq a b) = evalExpr a *> evalExpr b

compactKeys :: [Addr] -> (Addr -> Addr)
compactKeys keys = (M.!) $ M.fromList $ zip keys [0..]

runGC :: EvalState -> EvalState
runGC es@EvalState { gcStats = gcStats@GCStats { .. }, .. } =
  if allocsSinceGC < gcThreshold
    then es
    else
      let
        heap' = M.restrictKeys heap (getInnerAddrs heap env)
        f = compactKeys $ M.keys heap'
        heapCompacted = mapInnerAddrsScope f heap'
        envCompacted = mapInnerAddrsScope f env
      in
        EvalState
        { gcStats = gcStats { allocsSinceGC = 0, crrHeapSize = M.size heapCompacted }
        , env = envCompacted
        , heap = M.mapKeys f heapCompacted
        , ..
        }

getInnerAddrsVal :: Val -> Set Addr
getInnerAddrsVal (VRef a) = S.singleton a
getInnerAddrsVal (VRec _ m) = getInnerAddrsScope m
getInnerAddrsVal (VFun env _ _) = getInnerAddrsScope env
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
mapInnerAddrsVal f (VFun env i e) = VFun (mapInnerAddrsScope f env) i e
mapInnerAddrsVal _ v = v

mapInnerAddrsScope :: Functor f => (Addr -> Addr) -> f Val -> f Val
mapInnerAddrsScope f = (mapInnerAddrsVal f <$>)

runEval :: StateT EvalState (ExceptT EvalError IO) a -> EvalState -> IO ()
runEval m s = runExceptT (runStateT m s) >>= \case
  Left e -> print e
  Right _ -> pure ()

eval :: EvalState -> Expr -> IO ()
eval s e = runEval (evalExpr e) s
