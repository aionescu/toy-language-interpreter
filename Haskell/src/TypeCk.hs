module TypeCk(typeCheck) where

import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict(HashMap)

import AST

data VarState = Init | Uninit
  deriving Eq

type VarInfo = (Type, VarState)
type SymTypeTable = HashMap Ident VarInfo

data TypeError
  = ExpectedFound Type Type
  | UndeclaredVar Ident
  | VarAlreadyDeclared Ident
  | UninitializedVar Ident
  | TupleTooShort Type Int
  | ExpectedTupleFound Type

instance Show TypeError where
  show te = "Type error: " ++ go te ++ "."
    where
      go (ExpectedFound expected found) = "Expected " ++ show expected ++ ", but found " ++ show found
      go (UndeclaredVar ident) = "Variable " ++ ident ++ " was not declared"
      go (VarAlreadyDeclared ident) = "Variable " ++ ident ++ " has already been declared"
      go (UninitializedVar ident) = "Variable " ++ ident ++ " is not guaranteed to be initialized before its first read"
      go (TupleTooShort t i) = "The tuple type " ++ show t ++ " does not have enough elements to be indexed by the index " ++ show i
      go (ExpectedTupleFound found) = "Expected tuple type, but found " ++ show found

type TypeCk a = Either TypeError a

mustBe :: Type -> Type -> TypeCk ()
mustBe found expected
  | found == expected = pure ()
  | otherwise = throw $ ExpectedFound expected found

valType :: Val -> Type
valType (VBool _) = TBool
valType (VInt _) = TInt
valType (VTup vs) = TTup $ valType <$> vs

lookupVar :: Ident -> SymTypeTable -> TypeCk VarInfo
lookupVar var sym = maybe (throw $ UndeclaredVar var) pure $ M.lookup var sym

-- foldM :: (b -> a -> m b) -> b -> [a] -> m b
-- foldM _ b [] = pure b
-- foldM f b (a : as) = f b a >>= flip (foldM f) as

(!?) :: [a] -> Int -> Maybe a
(a : _) !? 0 = Just a
(_ : as) !? n = as !? (n - 1)
_ !? _ = Nothing

typeCheckExpr :: SymTypeTable -> Expr a -> TypeCk Type
typeCheckExpr _ (Lit v) = pure $ valType v
typeCheckExpr sym (Var ident) = do
  (type', state) <- lookupVar ident sym
  case state of
    Uninit -> throw $ UninitializedVar ident
    Init -> pure type'
typeCheckExpr sym (Arith a _ b) = do
  ta <- typeCheckExpr sym a
  ta `mustBe` TInt
  tb <- typeCheckExpr sym b
  tb `mustBe` TInt
  pure TInt
typeCheckExpr sym (Logic a _ b) = do
  ta <- typeCheckExpr sym a
  ta `mustBe` TBool
  tb <- typeCheckExpr sym b
  tb `mustBe` TBool
  pure TBool
typeCheckExpr sym (Comp a _ b) = do
  ta <- typeCheckExpr sym a
  tb <- typeCheckExpr sym b
  tb `mustBe` ta
  pure TBool
typeCheckExpr sym (TupLit vs) = TTup <$> traverse (typeCheckExpr sym) vs
typeCheckExpr sym (TupMember lhs idx) = do
  t <- typeCheckExpr sym lhs
  case t of
    TTup ts ->
      case ts !? idx of
        Nothing -> throw $ TupleTooShort t idx
        Just t' -> pure t'
    _ -> throw $ ExpectedTupleFound t
typeCheckExpr sym (With lhs updates) = do
  t <- typeCheckExpr sym lhs
  case t of
    TTup ts -> do
      tys <- traverse typeCheckUpdate updates
      mapM_ (checkMember ts) tys
      pure t
    _ -> throw $ ExpectedTupleFound t
  where
    typeCheckUpdate (idx, expr) = (idx, ) <$> typeCheckExpr sym expr
    checkMember :: [Type] -> (Int, Type) -> TypeCk ()
    checkMember ts (idx, t) =
      case ts !? idx of
        Nothing -> throw $ TupleTooShort (TTup ts) idx
        Just t' -> t `mustBe` t'

mergeVarInfo :: VarInfo -> VarInfo -> VarInfo
mergeVarInfo a b
  | a == b = a
  | otherwise = (fst a, Uninit)

typeCheckStmt :: SymTypeTable -> Stmt -> TypeCk SymTypeTable
typeCheckStmt sym Nop = pure sym
typeCheckStmt sym (Decl ident type') =
  case M.lookup ident sym of
    Just _ -> throw $ VarAlreadyDeclared ident
    Nothing -> pure $ M.insert ident (type', Uninit) sym
typeCheckStmt sym (Assign (Var ident) expr) = do
  (typ, _) <- lookupVar ident sym
  typ2 <- typeCheckExpr sym expr
  typ2 `mustBe` typ
  pure $ M.insert ident (typ, Init) sym
typeCheckStmt sym (Assign (TupMember lhs idx) expr) =
  typeCheckStmt sym $ Assign lhs (With lhs [(idx, expr)])
typeCheckStmt sym (DeclAssign ident (Just type') expr) = do
  sym2 <- typeCheckStmt sym (Decl ident type')
  typeCheckStmt sym2 (Assign (Var ident) expr)
typeCheckStmt sym (DeclAssign ident Nothing expr) = do
  t <- typeCheckExpr sym expr
  typeCheckStmt sym $ DeclAssign ident (Just t) expr
typeCheckStmt sym (Print e) = sym <$ typeCheckExpr sym e
typeCheckStmt sym (If cond then' else') = do
  tc <- typeCheckExpr sym cond
  tc `mustBe` TBool
  thenTbl <- typeCheckStmt sym then'
  elseTbl <- typeCheckStmt sym else'
  pure $
    M.intersectionWith mergeVarInfo thenTbl elseTbl `M.intersection` sym
typeCheckStmt sym (While cond body) = do
  tc <- typeCheckExpr sym cond
  tc `mustBe` TBool
  typeCheckStmt sym body
  pure sym
typeCheckStmt sym (Compound a b) = do
  sym2 <- typeCheckStmt sym a
  typeCheckStmt sym2 b

typeCheck :: Program -> TLI Program
typeCheck prog = toTLI $ prog <$ typeCheckStmt M.empty prog
