-- If we got to this point, we know typechecking succeeded, so we can use incomplete patterns
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module Language.TL.Eval(Val(..), ProgState(..), traverseSteps_, finalState, showOut, mkProgState) where

import Data.List(intercalate)
import Data.Functor(($>))
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M

import Language.TL.AST

data Val
  = VInt Integer
  | VBool Bool
  | VStr String
  | forall f. VRec (Field f) (Map f Val)
  | VFun (Val -> Eval Val)
  | VFile String

instance Show Val where
  show (VInt i) = show i
  show (VBool b) = show b
  show (VStr s) = show s
  show (VRec f m) = showFields False f " = " m
  show (VFun _) = "<λ>"
  show (VFile name) = "File<" ++ show name ++ ">"

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

data ProgState =
  ProgState
  { toDo :: ToDo
  , sym :: SymValTable
  , out :: Out
  , open :: FileTable
  , fs :: FileTable
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
      , "out = " ++ showL show (reverse out)
      ]
    where
      showM fmt fmtL m = withParens "{ " " }" (showVar fmt fmtL <$> M.toList m)
      showVar fmt fmtL (ident, var) = fmt ident ++ " = " ++ fmtL var

mkProgState :: FileTable -> Stmt -> ProgState
mkProgState fs stmt = ProgState { toDo = [stmt], sym = M.empty, out = [], open = M.empty, fs }

showOut :: ProgState -> String
showOut = unlines . reverse . (show <$>) . out

data EvalError
  = DivisionByZero
  | FileDoesNotExist String
  | FileAlreadyOpened String
  | FileAlreadyClosed String
  | FileNotOpened String
  | ReadDifferentType String Type Type
  | ReachedEOF String

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

type Eval a = Either EvalError a

evalExpr :: SymValTable -> Expr a -> Eval Val
evalExpr _ (Default t) = pure $ defaultVal t
evalExpr _ (IntLit i) = pure $ VInt i
evalExpr _ (BoolLit b) = pure $ VBool b
evalExpr _ (StrLit s) = pure $ VStr s
evalExpr sym (Var ident) = pure $ sym M.! ident
evalExpr sym (Arith a op b) = do
  a' <- evalExpr sym a
  b' <- evalExpr sym b
  case (b', op) of
    (VStr vb, Add) ->
      case a' of
        VStr va -> pure $ VStr $ va ++ vb
    (VInt 0, Divide) -> throw DivisionByZero
    (VInt 0, Remainder) -> throw DivisionByZero
    (VInt vb, _) ->
      case a' of
        VInt va -> pure $ VInt $ arithOp op va vb
evalExpr sym (Logic a op b) = do
  a' <- evalExpr sym a
  case (a', op) of
    (VBool True, Or) -> pure a'
    (VBool False, And) -> pure a'
    (VBool va, _) -> do
      b' <- evalExpr sym b
      case b' of
        VBool vb -> pure $ VBool $ logicOp op va vb
evalExpr sym (Comp a op b) = do
  a' <- evalExpr sym a
  b' <- evalExpr sym b
  pure $ VBool $ compOp op a' b'
evalExpr sym (RecLit f m) = VRec f <$> traverse (evalExpr sym) m
evalExpr sym (RecMember lhs f i) = do
  v <- evalExpr sym lhs
  case (v, f) of
    (VRec FRec m, FRec) -> pure $ m M.! i
    (VRec FTup m, FTup) -> pure $ m M.! i
evalExpr sym (RecWith lhs f us) = do
  v <- evalExpr sym lhs
  vals <- traverse (evalExpr sym) us
  case (v, f) of
    (VRec FRec fs, FRec) ->
      pure $ VRec FRec $ M.foldlWithKey' (\m k v' -> M.insert k v' m) fs vals
    (VRec FTup fs, FTup) ->
      pure $ VRec FTup $ M.foldlWithKey' (\m k v' -> M.insert k v' m) fs vals
evalExpr sym (RecUnion a b) = do
  ra <- evalExpr sym a
  rb <- evalExpr sym b
  case (ra, rb) of
    (VRec FRec a', VRec FRec b') -> pure $ VRec FRec $ M.union a' b'
evalExpr sym (Lam i _ e) = pure $ VFun \a -> evalExpr (M.insert i a sym) e
evalExpr sym (App f a) = do
  vf <- evalExpr sym f
  va <- evalExpr sym a
  case vf of
    VFun f' -> f' va

evalStmt :: ProgState -> Stmt -> Eval ProgState
evalStmt progState Nop = pure progState
evalStmt progState (Decl _ _) = pure progState
evalStmt ProgState{..} (Assign (Var ident) expr) = do
  v <- evalExpr sym expr
  pure $ ProgState { sym = M.insert ident v sym, .. }
evalStmt progState (Assign (RecMember lhs f i) expr) =
  evalStmt progState (Assign lhs (RecWith lhs f $ M.singleton i expr))
evalStmt ProgState{..} (DeclAssign ident (Just type') expr) =
  pure $ ProgState { toDo = Decl ident type' : Assign (Var ident) expr : toDo, .. }
evalStmt ProgState{..} (DeclAssign ident Nothing expr) =
  pure $ ProgState { toDo = Assign (Var ident) expr : toDo, .. }
evalStmt ProgState{..} (Print expr) = do
  v <- evalExpr sym expr
  pure $ ProgState { out = v : out, .. }
evalStmt ProgState{..} (If cond then' else') = do
  c' <- evalExpr sym cond
  case c' of
    VBool c -> pure $ ProgState { toDo = (if c then then' else else') : toDo, .. }
evalStmt ProgState{..} w@(While cond body) = do
  c' <- evalExpr sym cond
  case c' of
    VBool c -> pure $ ProgState { toDo = if c then body : w : toDo else toDo, .. }
evalStmt ProgState{..} (Compound a b) =
  pure $ ProgState { toDo = a : b : toDo, .. }
evalStmt ProgState{..} (Open ident expr) = do
  vf <- evalExpr sym expr
  case vf of
    VStr f ->
      case M.lookup f fs of
        Nothing -> throw $ FileDoesNotExist f
        Just content ->
          case M.lookup f open of
            Just _ -> throw $ FileAlreadyOpened f
            Nothing -> pure $ ProgState { sym = M.insert ident (VFile f) sym, open = M.insert f content open, .. }
evalStmt ProgState{..} (Read ident t expr) = do
  vf <- evalExpr sym expr
  case vf of
    VFile f ->
      case M.lookup f open of
        Nothing -> throw $ FileNotOpened f
        Just [] -> throw $ ReachedEOF f
        Just (c : cs) -> do
          let tc = valType c
          if tc /= t
            then throw $ ReadDifferentType f tc t
            else pure $ ProgState { sym = M.insert ident c sym, open = M.insert f cs open, .. }
evalStmt ProgState{..} (Close expr) = do
  vf <- evalExpr sym expr
  case vf of
    VFile f ->
      case M.lookup f open of
        Nothing -> throw $ FileAlreadyClosed f
        Just _ -> pure $ ProgState { open = M.delete f open, .. }

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
