-- If we got to this point, we know typechecking succeeded, so we can use incomplete patterns
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module Language.TL.Eval(ProgState(..), traverseSteps_, finalState, showOut) where

import Data.Functor(($>))
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M

import Language.TL.AST

data Val
  = VInt Int
  | VBool Bool
  | forall f. VRec (Field f) (Map f Val)
  | VFun (Val -> Eval Val)

instance Show Val where
  show (VInt i) = show i
  show (VBool b) = show b
  show (VRec f m) = showFields False f "<-" m
  show (VFun _) = "<λ>"

instance Ord Val where
  compare (VInt a) (VInt b) = compare a b
  compare (VBool a) (VBool b) = compare a b
  compare (VRec FRec a) (VRec FRec b) = compare a b
  compare (VRec FTup a) (VRec FTup b) = compare a b
  compare (VFun _) _ = error "Functions are not comparable. Did you run the typechecker?"
  compare _ (VFun _) = error "Functions are not comparable. Did you run the typechecker?"
  compare _ _ = error "Type mismatch. Did you run the typechecker?"

instance Eq Val where
  (==) = ((== EQ) .) . compare

type SymValTable = Map Ident Val
type ToDo = [Stmt]
type Out = [Val]

data ProgState =
  ProgState
  { toDo :: ToDo
  , sym :: SymValTable
  , out :: Out
  }

instance Show ProgState where
  show ProgState{..} = unlines ["toDo = " ++ show toDo, "sym = " ++ sym', "out = " ++ show (reverse out)]
    where
      sym' = withParens "{ " " }" (showVar <$> M.toList sym)
      showVar (ident, var) = ident ++ " <- " ++ show var

mkProgState :: Stmt -> ProgState
mkProgState stmt = ProgState { toDo = [stmt], sym = M.empty, out = [] }

showOut :: ProgState -> String
showOut = unlines . reverse . (show <$>) . out

data EvalError = DivisionByZero

instance Show EvalError where
  show te = "Eval error: " ++ go te ++ "."
    where
      go DivisionByZero = "An attempt was made to divide by zero"

type Eval a = Either EvalError a

evalExpr :: SymValTable -> Expr a -> Eval Val
evalExpr _ (IntLit i) = pure $ VInt i
evalExpr _ (BoolLit b) = pure $ VBool b
evalExpr sym (Var ident) = pure $ sym M.! ident
evalExpr sym (Arith a op b) = do
  a' <- evalExpr sym a
  b' <- evalExpr sym b
  case (b', op) of
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

smallStep :: ProgState -> Maybe (Eval ProgState)
smallStep ProgState { toDo = [] } = Nothing
smallStep ProgState { toDo = stmt : toDo, .. } = Just $ evalStmt ProgState{..} stmt

traverseSteps_' :: Applicative f => (String -> f a) -> ProgState -> f ()
traverseSteps_' f state =
  f (show state) *>
    case smallStep state of
      Nothing -> pure ()
      Just (Left e) -> f (show e) $> ()
      Just (Right state') -> traverseSteps_' f state'

traverseSteps_ :: Applicative f => (String -> f a) -> Program -> f ()
traverseSteps_ f = traverseSteps_' f . mkProgState

finalState' :: ProgState -> Eval ProgState
finalState' s =
  case smallStep s of
    Nothing -> pure s
    Just s' -> s' >>= finalState'

finalState :: Program -> TLI ProgState
finalState = toTLI . finalState' . mkProgState
