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

setN :: [a] -> Int -> a -> [a]
setN [] _ _ = []
setN (_ : as) 0 v  = v : as
setN (a : as) n v = a : setN as (pred n) v

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
  b' <- evalExpr sym b
  case (a', b') of
    (VBool va, VBool vb) -> pure $ VBool $ logicOp op va vb
evalExpr sym (Comp a op b) = do
  va <- evalExpr sym a
  vb <- evalExpr sym b
  case (va, vb) of
    (VInt a', VInt b') -> pure $ VBool $ compOp op a' b'
    (VBool a', VBool b') -> pure $ VBool $ compOp op a' b'
evalExpr sym (TupLit t) = VTup <$> traverse (evalExpr sym) t
evalExpr sym (RecordLit t) = VRecord <$> traverseM (evalExpr sym) t
evalExpr sym (TupMember lhs idx) = do
  v <- evalExpr sym lhs
  case v of
    VTup vs -> pure $ vs !! idx
evalExpr sym (RecordMember lhs ident) = do
  v <- evalExpr sym lhs
  case v of
    VRecord vs -> pure $ vs M.! ident
evalExpr sym (TupWith lhs updates) = do
  v <- evalExpr sym lhs
  case v of
    VTup vs -> do
      vals <- traverseM (evalExpr sym) updates
      pure $ VTup $ M.foldlWithKey' setN vs vals
evalExpr sym (RecordWith lhs updates) = do
  v <- evalExpr sym lhs
  case v of
    VRecord vs -> do
      vals <- traverseM (evalExpr sym) updates
      pure $ VRecord $ M.foldlWithKey' (\m k v' -> M.insert k v' m) vs vals
evalExpr sym (RecordUnion a b) = do
  ra <- evalExpr sym a
  rb <- evalExpr sym b
  case (ra, rb) of
    (VRecord a', VRecord b') -> pure $ VRecord $ M.union a' b'

evalStmt :: ProgState -> Stmt -> Eval ProgState
evalStmt progState Nop = pure progState
evalStmt progState (Decl _ _) = pure progState
evalStmt ProgState{..} (Assign (Var ident) expr) = do
  v <- evalExpr sym expr
  pure $ ProgState {sym =  M.insert ident v sym, .. }
evalStmt progState (Assign (TupMember lhs idx) expr) =
  evalStmt progState (Assign lhs (TupWith lhs $ M.singleton idx expr))
evalStmt progState (Assign (RecordMember lhs ident) expr) =
  evalStmt progState (Assign lhs (RecordWith lhs $ M.singleton ident expr))
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
