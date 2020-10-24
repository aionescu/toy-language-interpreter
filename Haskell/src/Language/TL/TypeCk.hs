module Language.TL.TypeCk(typeCheck) where

import qualified Data.Map.Strict as M
import Data.Map.Strict(Map)

import Language.TL.AST
import Data.Function (on)

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
  NoFieldInRec :: Type -> (Field a) -> TypeError
  NeedRecordTypesForUnion :: TypeError
  DuplicateIncompatibleField :: Ident -> TypeError
  TupRecMismatch :: TypeError

instance Show TypeError where
  show te = "Type error: " ++ go te ++ "."
    where
      go (ExpectedFound expected found) = "Expected " ++ show expected ++ ", but found " ++ show found
      go (UndeclaredVar ident) = "Variable " ++ ident ++ " was not declared"
      go (VarAlreadyDeclared ident) = "Variable " ++ ident ++ " has already been declared"
      go (UninitializedVar ident) = "Variable " ++ ident ++ " is not guaranteed to be initialized before its first read"
      go (ExpectedRecFound found) = "Expected tuple or record type, but found " ++ show found
      go (NoFieldInRec t (FRec i)) = "The record type " ++ show t ++ " has no field named " ++ i
      go (NoFieldInRec t (FTup i)) = "The tuple type " ++ show t ++ " does not have enough elements to be indexed by the index " ++ show i
      go NeedRecordTypesForUnion = "Both operands of the \"|\" operator must be of record types"
      go (DuplicateIncompatibleField i) = "The field " ++ i ++ " appears twice in the union, but with different types"
      go TupRecMismatch = "Can't mix tuple and record update/access"

type TypeCk a = Either TypeError a

mustBe :: Type -> Type -> TypeCk ()
mustBe found expected
  | found == expected = pure ()
  | otherwise = throw $ ExpectedFound expected found

valType :: Val -> Type
valType (VBool _) = TBool
valType (VInt _) = TInt
valType (VRec (FsRec fs)) = TRec $ FsRec $ valType <$> fs
valType (VRec (FsTup fs)) = TRec $ FsTup $ valType <$> fs

lookupVar :: Ident -> SymTypeTable -> TypeCk VarInfo
lookupVar var sym = maybe (throw $ UndeclaredVar var) pure $ M.lookup var sym

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
typeCheckExpr sym (RecLit (FsRec fs)) = TRec . FsRec <$> traverse (typeCheckExpr sym) fs
typeCheckExpr sym (RecLit (FsTup fs)) = TRec . FsTup <$> traverse (typeCheckExpr sym) fs
typeCheckExpr sym (RecMember lhs f) = do
  t <- typeCheckExpr sym lhs
  case t of
    TRec fs ->
      case (f, fs) of
        (FRec _, FsRec fs') ->
          case M.lookup f fs' of
            Nothing -> throw $ NoFieldInRec t f
            Just t' -> pure t'
        (FTup _, FsTup fs') ->
          case M.lookup f fs' of
            Nothing -> throw $ NoFieldInRec t f
            Just t' -> pure t'
        _ -> throw TupRecMismatch
    _ -> throw $ ExpectedRecFound t
typeCheckExpr sym (RecWith lhs updates) = do
  t <- typeCheckExpr sym lhs
  case t of
    TRec fs ->
      case (fs, updates) of
        (FsRec fs', FsRec us) -> do
          tys <-  traverseM (typeCheckExpr sym) us
          M.traverseWithKey (checkMember t fs') tys
          pure t
        (FsTup fs', FsTup us) -> do
          tys <-  traverseM (typeCheckExpr sym) us
          M.traverseWithKey (checkMember t fs') tys
          pure t
        _ -> throw TupRecMismatch
    _ -> throw $ ExpectedRecFound t
  where
    checkMember :: Type -> Map (Field a) Type -> Field a -> Type -> TypeCk ()
    checkMember rec fs f t =
      case M.lookup f fs of
        Nothing -> throw $ NoFieldInRec rec f
        Just t' -> t `mustBe` t'
typeCheckExpr sym (RecUnion a b) = do
  ta <- typeCheckExpr sym a
  tb <- typeCheckExpr sym b

  case (ta, tb) of
    (TRec (FsRec fsA), TRec (FsRec fsB)) -> do
       (TRec . FsRec) <$> M.traverseWithKey throwIfDup ((M.unionWith checkDup `on` (Just <$>)) fsA fsB)
    _ -> throw $ NeedRecordTypesForUnion
  where
    checkDup a' b'
      | a' == b' = a'
      | otherwise = Nothing

    throwIfDup :: Field Ident -> Maybe Type -> TypeCk Type
    throwIfDup (FRec ident) Nothing = throw $ DuplicateIncompatibleField ident
    throwIfDup _ (Just t) = pure t

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
typeCheckStmt sym (Assign (RecMember lhs f@(FRec _)) expr) =
  typeCheckStmt sym $ Assign lhs $ RecWith lhs $ FsRec $ M.singleton f expr
typeCheckStmt sym (Assign (RecMember lhs f@(FTup _)) expr) =
  typeCheckStmt sym $ Assign lhs $ RecWith lhs $ FsTup $ M.singleton f expr
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
