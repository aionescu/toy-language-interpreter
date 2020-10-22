-- If we got to this point, we know typechecking succeeded, so we can use incomplete patterns
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module Eval(allSteps, eval, ProgState(..), showSteps, showOut) where

import qualified Data.Map.Strict as M
import Data.Map.Strict(Map)

import AST

type SymValTable = Map Ident Val
type ToDo = [Stmt]
type Out = [String]

data ProgState =
  ProgState
  { toDo :: ToDo
  , sym :: SymValTable
  , out :: Out
  }

instance Show ProgState where
  show ProgState{..} = unlines ["toDo = " ++ show toDo, "sym = " ++ sym', "out = " ++ show out]
    where
      sym' = withParens "{ " " }" (showVar <$> M.toList sym)
      showVar (ident, var) = ident ++ " <- " ++ show var

mkProgState :: Stmt -> ProgState
mkProgState stmt = ProgState { toDo = [stmt], sym = M.empty, out = [] }

showSteps :: [ProgState] -> String
showSteps = unlines . (show <$>)

showOut :: ProgState -> String
showOut = unlines . reverse . out

data EvalError = DivisionByZero

instance Show EvalError where
  show te = "Eval error: " ++ go te ++ "."
    where
      go DivisionByZero = "An attempt was made to divide by zero"

type Eval a = Either EvalError a

evalExpr :: SymValTable -> Expr a -> Eval Val
evalExpr _ (Lit v) = pure v
evalExpr sym (Var ident) = pure $ sym M.! ident
evalExpr sym (Arith a op b) = do
  a' <- evalExpr sym a
  b' <- evalExpr sym b
  case (a', b') of
    (VInt va, VInt vb) ->
      if vb == 0 && (op == Divide || op == Remainder)
        then throw $ DivisionByZero
        else pure $ VInt $ arithOp op va vb
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
  va <- evalExpr sym a
  vb <- evalExpr sym b
  case (va, vb) of
    (VInt a', VInt b') -> pure $ VBool $ compOp op a' b'
    (VBool a', VBool b') -> pure $ VBool $ compOp op a' b'
evalExpr sym (RecLit (FsRec fs)) = VRec . FsRec <$> traverse (evalExpr sym) fs
evalExpr sym (RecLit (FsTup fs)) = VRec . FsTup <$> traverse (evalExpr sym) fs
evalExpr sym (RecMember lhs f@(FRec _)) = do
  v <- evalExpr sym lhs
  case v of
    VRec (FsRec fs) -> pure $ fs M.! f
evalExpr sym (RecMember lhs f@(FTup _)) = do
  v <- evalExpr sym lhs
  case v of
    VRec (FsTup fs) -> pure $ fs M.! f
evalExpr sym (RecWith lhs (FsRec us)) = do
  v <- evalExpr sym lhs
  case v of
    VRec (FsRec fs) -> do
      vals <- traverseM (evalExpr sym) us
      pure $ VRec $ FsRec $ M.foldlWithKey' (\m k v' -> M.insert k v' m) fs vals
evalExpr sym (RecWith lhs (FsTup us)) = do
  v <- evalExpr sym lhs
  case v of
    VRec (FsTup fs) -> do
      vals <- traverseM (evalExpr sym) us
      pure $ VRec $ FsTup $ M.foldlWithKey' (\m k v' -> M.insert k v' m) fs vals
evalExpr sym (RecUnion a b) = do
  ra <- evalExpr sym a
  rb <- evalExpr sym b
  case (ra, rb) of
    (VRec (FsRec a'), VRec (FsRec b')) -> pure $ VRec $ FsRec $ M.union a' b'

evalStmt :: ProgState -> Stmt -> Eval ProgState
evalStmt progState Nop = pure progState
evalStmt progState (Decl _ _) = pure progState
evalStmt ProgState{..} (Assign (Var ident) expr) = do
  v <- evalExpr sym expr
  pure $ ProgState {sym =  M.insert ident v sym, .. }
evalStmt progState (Assign (RecMember lhs f@(FRec _)) expr) =
  evalStmt progState (Assign lhs (RecWith lhs $ FsRec $ M.singleton f expr))
evalStmt progState (Assign (RecMember lhs f@(FTup _)) expr) =
  evalStmt progState (Assign lhs (RecWith lhs $ FsTup $ M.singleton f expr))
evalStmt ProgState{..} (DeclAssign ident (Just type') expr) =
  pure $ ProgState { toDo = Decl ident type' : Assign (Var ident) expr : toDo, .. }
evalStmt ProgState{..} (DeclAssign ident Nothing expr) =
  pure $ ProgState { toDo = Assign (Var ident) expr : toDo, .. }
evalStmt ProgState{..} (Print expr) = do
  v <- evalExpr sym expr
  pure $ ProgState { out = show v : out, .. }
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

allSteps' :: ProgState -> Eval [ProgState]
allSteps' state =
  case smallStep state of
    Nothing -> pure [state]
    Just result -> do
      state' <- result
      states <- allSteps' state'
      pure $ state : states

allSteps :: Program -> TLI [ProgState]
allSteps prog = toTLI $ allSteps' $ mkProgState prog

eval :: Program -> TLI ProgState
eval p = last <$> allSteps p
