-- If we got to this point, we know typechecking succeeded, so we can use incomplete patterns
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module Language.TL.Eval(Val(..), ProgState(..), traverseSteps_, finalState, showOut, mkProgState) where

import Numeric(showHex)
import Data.List(intercalate)
import Data.Functor(($>))
import Control.Monad(when)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Data.Set(Set)
import qualified Data.Set as S

import Language.TL.AST

type Addr = Int

data Val
  = VInt Integer
  | VBool Bool
  | VStr String
  | forall f. VRec (Field f) (Map f Val)
  | VFun SymValTable (Val -> Eval Val)
  | VRef Addr

showH :: (Integral a, Show a) => a -> String
showH a = "0x" ++ showHex a ""

instance Show Val where
  show (VInt i) = show i
  show (VBool b) = show b
  show (VStr s) = show s
  show (VRec f m) = showFields False f " = " m
  show (VFun _ _) = "<λ>"
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

data ProgState =
  ProgState
  { fs :: FileTable
  , open :: FileTable
  , toDo :: ToDo
  , sym :: SymValTable
  , crrHeapSize :: Int
  , maxHeapSize :: Int
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
      , "heap[" ++ show crrHeapSize ++ " / " ++ show maxHeapSize ++ "] = " ++ showM showH show heap
      , "out = " ++ showL show (reverse out)
      ]
    where
      showM fmt fmtL m = withParens "{ " " }" (showVar fmt fmtL <$> M.toList m)
      showVar fmt fmtL (ident, var) = fmt ident ++ " = " ++ fmtL var

showOut :: ProgState -> String
showOut = unlines . reverse . (show <$>) . out

mkProgState :: FileTable -> Int -> Stmt -> ProgState
mkProgState fs maxHeapSize stmt =
  ProgState
  { fs
  , open = M.empty
  , toDo = [stmt]
  , sym = M.empty
  , out = []
  , crrHeapSize = 0
  , maxHeapSize
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
      go (FileDoesNotExist f) = "The file " ++ show f ++ " does not exist in the filesystem."
      go (FileAlreadyOpened f) = "The file " ++ show f ++ " has already been opened."
      go (FileAlreadyClosed f) = "The file " ++ show f ++ " has been closed."
      go (FileNotOpened f) = "The file " ++ show f ++ " has not been opened."
      go (ReadDifferentType f found expected) =
        "In file " ++ show f
        ++ ", found a value of type " ++ show found
        ++ ", but expected a value of type " ++ show expected
      go (ReachedEOF f) = "There are no more values to read in file " ++ show f
      go (OutOfMemory size) = "Out of memory (heap size: " ++ show size ++ ")"

type Eval a = Either EvalError a

evalExpr :: SymValTable -> Heap -> Expr a -> Eval Val
evalExpr _ _ (Default t) = pure $ defaultVal t
evalExpr _ _ (IntLit i) = pure $ VInt i
evalExpr _ _ (BoolLit b) = pure $ VBool b
evalExpr _ _ (StrLit s) = pure $ VStr s
evalExpr sym _ (Var ident) = pure $ sym M.! ident
evalExpr sym heap (Arith a op b) = do
  a' <- evalExpr sym heap a
  b' <- evalExpr sym heap b
  case (b', op) of
    (VStr vb, Add) ->
      case a' of
        VStr va -> pure $ VStr $ va ++ vb
    (VInt 0, Divide) -> throw DivisionByZero
    (VInt 0, Remainder) -> throw DivisionByZero
    (VInt vb, _) ->
      case a' of
        VInt va -> pure $ VInt $ arithOp op va vb
evalExpr sym heap (Logic a op b) = do
  a' <- evalExpr sym heap a
  case (a', op) of
    (VBool True, Or) -> pure a'
    (VBool False, And) -> pure a'
    (VBool va, _) -> do
      b' <- evalExpr sym heap b
      case b' of
        VBool vb -> pure $ VBool $ logicOp op va vb
evalExpr sym heap (Comp a op b) = do
  a' <- evalExpr sym heap a
  b' <- evalExpr sym heap b
  pure $ VBool $ compOp op a' b'
evalExpr sym heap (RecLit f m) = VRec f <$> traverse (evalExpr sym heap) m
evalExpr sym heap (RecMember lhs f i) = do
  v <- evalExpr sym heap lhs
  case (v, f) of
    (VRec FRec m, FRec) -> pure $ m M.! i
    (VRec FTup m, FTup) -> pure $ m M.! i
evalExpr sym heap (RecWith lhs f us) = do
  v <- evalExpr sym heap lhs
  vals <- traverse (evalExpr sym heap) us
  case (v, f) of
    (VRec FRec fs, FRec) ->
      pure $ VRec FRec $ M.foldlWithKey' (\m k v' -> M.insert k v' m) fs vals
    (VRec FTup fs, FTup) ->
      pure $ VRec FTup $ M.foldlWithKey' (\m k v' -> M.insert k v' m) fs vals
evalExpr sym heap (RecUnion a b) = do
  ra <- evalExpr sym heap a
  rb <- evalExpr sym heap b
  case (ra, rb) of
    (VRec FRec a', VRec FRec b') -> pure $ VRec FRec $ M.union a' b'
evalExpr sym heap (Lam i _ e) = pure $ VFun sym \a -> evalExpr (M.insert i a sym) heap e
evalExpr sym heap (App f a) = do
  vf <- evalExpr sym heap f
  va <- evalExpr sym heap a
  case vf of
    VFun _ f' -> f' va
evalExpr sym heap (Deref e) = do
  v <- evalExpr sym heap e
  case v of
    VRef addr -> pure $ heap M.! addr

evalStmt :: ProgState -> Stmt -> Eval ProgState
evalStmt progState Nop = pure progState
evalStmt progState (Decl _ _) = pure progState
evalStmt ProgState{..} (Assign (Var ident) expr) = do
  v <- evalExpr sym heap expr
  pure $ ProgState { sym = M.insert ident v sym, .. }
evalStmt progState (Assign (RecMember lhs f i) expr) =
  evalStmt progState (Assign lhs (RecWith lhs f $ M.singleton i expr))
evalStmt ProgState{..} (DeclAssign ident (Just type') expr) =
  pure $ ProgState { toDo = Decl ident type' : Assign (Var ident) expr : toDo, .. }
evalStmt ProgState{..} (DeclAssign ident Nothing expr) =
  pure $ ProgState { toDo = Assign (Var ident) expr : toDo, .. }
evalStmt ProgState{..} (Print expr) = do
  v <- evalExpr sym heap expr
  pure $ ProgState { out = v : out, .. }
evalStmt ProgState{..} (If cond then' else') = do
  c' <- evalExpr sym heap cond
  case c' of
    VBool c -> pure $ ProgState { toDo = (if c then then' else else') : toDo, .. }
evalStmt ProgState{..} w@(While cond body) = do
  c' <- evalExpr sym heap cond
  case c' of
    VBool c -> pure $ ProgState { toDo = if c then body : w : toDo else toDo, .. }
evalStmt ProgState{..} (Compound a b) =
  pure $ ProgState { toDo = a : b : toDo, .. }
evalStmt ProgState{..} (Open expr) = do
  vf <- evalExpr sym heap expr
  case vf of
    VStr f ->
      case M.lookup f fs of
        Nothing -> throw $ FileDoesNotExist f
        Just content ->
          case M.lookup f open of
            Just _ -> throw $ FileAlreadyOpened f
            Nothing -> pure $ ProgState { open = M.insert f content open, .. }
evalStmt ProgState{..} (Read ident t expr) = do
  vf <- evalExpr sym heap expr
  case vf of
    VStr f ->
      case M.lookup f open of
        Nothing -> throw $ FileNotOpened f
        Just [] -> throw $ ReachedEOF f
        Just (c : cs) -> do
          let tc = valType c
          if tc /= t
            then throw $ ReadDifferentType f tc t
            else pure $ ProgState { sym = M.insert ident c sym, open = M.insert f cs open, .. }
evalStmt ProgState{..} (Close expr) = do
  vf <- evalExpr sym heap expr
  case vf of
    VStr f ->
      case M.lookup f open of
        Nothing -> throw $ FileAlreadyClosed f
        Just _ -> pure $ ProgState { open = M.delete f open, .. }
evalStmt ProgState{..} (New i e) = do
  when (crrHeapSize == maxHeapSize)
    $ throw $ OutOfMemory maxHeapSize
  v <- evalExpr sym heap e
  let sym' = M.insert i (VRef crrHeapSize) sym
  let heap' = M.insert crrHeapSize v heap
  pure $
    ProgState
    { sym = sym'
    , heap = M.restrictKeys heap' (getInnerAddrs heap' sym')
    , crrHeapSize = succ crrHeapSize
    , ..
    }
evalStmt ProgState{..} (WriteAt lhs rhs) = do
  vl <- evalExpr sym heap lhs
  vr <- evalExpr sym heap rhs
  case vl of
    VRef addr -> pure $ ProgState { heap = M.insert addr vr heap, .. }

getInnerAddrsVal :: Val -> Set Addr
getInnerAddrsVal (VRef a) = S.singleton a
getInnerAddrsVal (VRec _ m) = getInnerAddrsScope m
getInnerAddrsVal (VFun sym _) = getInnerAddrsScope sym
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

smallStep :: ProgState -> Maybe (Eval ProgState)
smallStep ProgState { toDo = [] } = Nothing
smallStep ProgState { toDo = stmt : toDo, .. } = Just $ evalStmt ProgState{..} stmt

traverseSteps_ :: Applicative f => (String -> f a) -> ProgState -> f ()
traverseSteps_ f state =
  f (show state) *>
    case smallStep state of
      Nothing -> pure ()
      Just (Left e) -> f (show e) $> ()
      Just (Right state') -> traverseSteps_ f state'

finalState' :: ProgState -> Eval ProgState
finalState' s =
  case smallStep s of
    Nothing -> pure s
    Just s' -> s' >>= finalState'

finalState :: ProgState -> TLI ProgState
finalState = toTLI . finalState'
