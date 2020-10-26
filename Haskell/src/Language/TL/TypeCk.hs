module Language.TL.TypeCk(typeCheck) where

import qualified Data.Map.Strict as M
import Data.Map.Strict(Map)

import Language.TL.AST
import Data.Function (on)
import Data.Functor (($>))

data VarState = Init | Uninit
  deriving Eq

type VarInfo = (Type, VarState)
type SymTypeTable = Map Ident VarInfo

data TypeError :: * where
  ExpectedFound :: Type -> Type -> TypeError
  UndeclaredVar :: Ident -> TypeError
  VarAlreadyDeclared :: Ident -> TypeError
  UninitializedVar :: Ident -> TypeError
  ExpectedRecFound :: Type -> TypeError
  NoFieldInRec :: Type -> Field f -> f -> TypeError
  NeedRecordTypesForUnion :: TypeError
  DuplicateIncompatibleField :: Ident -> TypeError
  ExpectedFunFound :: Type -> TypeError
  CantShadow :: Ident -> TypeError

instance Show TypeError where
  show te = "Type error: " ++ go te ++ "."
    where
      go (ExpectedFound expected found) = "Expected " ++ show expected ++ ", but found " ++ show found
      go (UndeclaredVar ident) = "Variable " ++ ident ++ " was not declared"
      go (VarAlreadyDeclared ident) = "Variable " ++ ident ++ " has already been declared"
      go (UninitializedVar ident) = "Variable " ++ ident ++ " is not guaranteed to be initialized before its first read"
      go (ExpectedRecFound t@(TRec FRec _)) = "Expected tuple type, but found record type " ++ show t
      go (ExpectedRecFound t@(TRec FTup _)) = "Expected record type, but found tuple type " ++ show t
      go (ExpectedRecFound t) = "Expected tuple or record type, but found " ++ show t
      go (NoFieldInRec t FRec i) = "The record type " ++ show t ++ " has no field named " ++ i
      go (NoFieldInRec t FTup i) = "The tuple type " ++ show t ++ " does not have enough elements to be indexed by the index " ++ show i
      go NeedRecordTypesForUnion = "Both operands of the \"|\" operator must be of record types"
      go (DuplicateIncompatibleField i) = "The field " ++ i ++ " appears twice in the union, but with different types"
      go (ExpectedFunFound t) = "Expected function type, but found " ++ show t
      go (CantShadow i) = "Lambda argument cannot shadow existing variable " ++ i

type TypeCk a = Either TypeError a

mustBe :: Type -> Type -> TypeCk ()
mustBe found expected
  | found == expected = pure ()
  | otherwise = throw $ ExpectedFound expected found

lookupVar :: Ident -> SymTypeTable -> TypeCk VarInfo
lookupVar var sym = maybe (throw $ UndeclaredVar var) pure $ M.lookup var sym

typeCheckExpr :: SymTypeTable -> Expr a -> TypeCk Type
typeCheckExpr _ (IntLit _) = pure TInt
typeCheckExpr _ (BoolLit _) = pure TBool
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
typeCheckExpr sym (RecLit f m) = TRec f <$> traverse (typeCheckExpr sym) m
typeCheckExpr sym (RecMember lhs f i) = do
  t <- typeCheckExpr sym lhs
  case (t, f) of
    (TRec FRec m, FRec) ->
      case M.lookup i m of
        Nothing -> throw $ NoFieldInRec t f i
        Just t' -> pure t'
    (TRec FTup m, FTup) ->
      case M.lookup i m of
        Nothing -> throw $ NoFieldInRec t f i
        Just t' -> pure t'
    _ -> throw $ ExpectedRecFound t
typeCheckExpr sym (RecWith lhs f us) = do
  t <- typeCheckExpr sym lhs
  tys <- traverseM (typeCheckExpr sym) us
  case (t, f) of
    (TRec FRec m, FRec) -> do
      M.traverseWithKey (checkMember t f m) tys $> t
    (TRec FTup m, FTup) -> do
      M.traverseWithKey (checkMember t f m) tys $> t
    _ -> throw $ ExpectedRecFound t
  where
    checkMember :: Ord f => Type -> Field f -> Map f Type -> f -> Type -> TypeCk ()
    checkMember rec f' m i t =
      case M.lookup i m of
        Nothing -> throw $ NoFieldInRec rec f' i
        Just t' -> t `mustBe` t'
typeCheckExpr sym (RecUnion a b) = do
  ta <- typeCheckExpr sym a
  tb <- typeCheckExpr sym b
  case (ta, tb) of
    (TRec FRec as, TRec FRec bs) -> do
       (TRec FRec) <$> M.traverseWithKey throwIfDup ((M.unionWith checkDup `on` (Just <$>)) as bs)
    _ -> throw $ NeedRecordTypesForUnion
  where
    checkDup a' b'
      | a' == b' = a'
      | otherwise = Nothing

    throwIfDup :: Ident -> Maybe Type -> TypeCk Type
    throwIfDup ident Nothing = throw $ DuplicateIncompatibleField ident
    throwIfDup _ (Just t) = pure t
typeCheckExpr sym (Lam i t e) = do
  case M.lookup i sym of
    Just _ -> throw $ CantShadow i
    Nothing -> (TFun t) <$> typeCheckExpr (M.insert i (t, Init) sym) e
typeCheckExpr sym (App f a) = do
  tf <- typeCheckExpr sym f
  ta <- typeCheckExpr sym a
  case tf of
    TFun i o -> ta `mustBe` i $> o
    _ -> throw $ ExpectedFunFound tf

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
typeCheckStmt sym (Assign (RecMember lhs f i) expr) =
  typeCheckStmt sym $ Assign lhs $ RecWith lhs f $ M.singleton i expr
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
  typeCheckStmt sym body $> sym
typeCheckStmt sym (Compound a b) = do
  sym2 <- typeCheckStmt sym a
  typeCheckStmt sym2 b

typeCheck :: Program -> TLI Program
typeCheck prog = toTLI $ prog <$ typeCheckStmt M.empty prog
