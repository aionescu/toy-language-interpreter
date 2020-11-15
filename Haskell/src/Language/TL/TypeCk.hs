module Language.TL.TypeCk(typeCheck) where

import Data.Function(on)
import Data.Functor(($>))
import Control.Monad(when)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M

import Language.TL.AST

data VarState = Init | Uninit
  deriving Eq

type VarInfo = (Type, VarState)
type SymTypeTable = Map Ident VarInfo

data TypeError
  = ExpectedFound Type Type
  | UndeclaredVar Ident
  | VarAlreadyDeclared Ident
  | UninitializedVar Ident
  | ExpectedRecFound Type
  | forall f. NoFieldInRec Type (Field f) f
  | NeedRecordTypesForUnion
  | DuplicateIncompatibleField Ident
  | ExpectedFunFound Type
  | CantShadow Ident
  | TypeIsOpaque Type
  | CanOnlyAppendStrings
  | CanOnlyAddNumegers
  | ExpectedRefFound Type

instance Show TypeError where
  show te = "Type error: " ++ go te ++ "."
    where
      go (ExpectedFound expected found) = "Expected " ++ show expected ++ ", but found " ++ show found
      go (UndeclaredVar ident) = "Variable " ++ ident ++ " was not declared"
      go (VarAlreadyDeclared ident) = "Variable " ++ ident ++ " has already been declared"
      go (UninitializedVar ident) = "Variable " ++ ident ++ " is not guaranteed to be initialized before it is used"
      go (ExpectedRecFound t@(TRec FRec _)) = "Expected tuple type, but found record type " ++ show t
      go (ExpectedRecFound t@(TRec FTup _)) = "Expected record type, but found tuple type " ++ show t
      go (ExpectedRecFound t) = "Expected tuple or record type, but found " ++ show t
      go (NoFieldInRec t FRec i) = "The record type " ++ show t ++ " has no field named " ++ i
      go (NoFieldInRec t FTup i) = "The tuple type " ++ show t ++ " does not have enough elements to be indexed by the index " ++ show i
      go NeedRecordTypesForUnion = "Both operands of the \"&\" operator must be of record types"
      go (DuplicateIncompatibleField i) = "The field " ++ i ++ " appears twice in the union, but with different types"
      go (ExpectedFunFound t) = "Expected function type, but found " ++ show t
      go (CantShadow i) = "Lambda argument cannot shadow existing variable " ++ i
      go (TypeIsOpaque t) = "The type " ++ show t ++ " is opaque"
      go CanOnlyAppendStrings = "Both operands of the append operation must be strings"
      go CanOnlyAddNumegers = "Both operands of the addition operation must be integers"
      go (ExpectedRefFound t) = "Expected reference type, but found type " ++ show t

type TypeCk a = Either TypeError a

unwrapTRef :: Type -> TypeCk Type
unwrapTRef (TRef t) = pure t
unwrapTRef t = throw $ ExpectedRefFound t

mustBe :: Type -> Type -> TypeCk ()
mustBe found expected
  | found == expected = pure ()
  | otherwise = throw $ ExpectedFound expected found

lookupVar :: Ident -> SymTypeTable -> TypeCk VarInfo
lookupVar var sym = maybe (throw $ UndeclaredVar var) pure $ M.lookup var sym

typeCheckExpr :: SymTypeTable -> Expr a -> TypeCk Type
typeCheckExpr _ (Default t) = do
  when (isOpaque t)
    $ throw $ TypeIsOpaque t
  pure t

typeCheckExpr _ (NumLit _) = pure TNum
typeCheckExpr _ (BoolLit _) = pure TBool
typeCheckExpr _ (StrLit _) = pure TStr

typeCheckExpr sym (Var ident) = do
  (type', state) <- lookupVar ident sym
  case state of
    Uninit -> throw $ UninitializedVar ident
    Init -> pure type'

typeCheckExpr sym (Arith a Add b) = do
  ta <- typeCheckExpr sym a
  tb <- typeCheckExpr sym b
  case (ta, tb) of
    (TNum, TNum) -> pure TNum
    (TStr, TStr) -> pure TStr
    _ -> throw $
      if TStr `elem` [ta, tb]
        then CanOnlyAppendStrings
        else CanOnlyAddNumegers

typeCheckExpr sym (Arith a _ b) = do
  ta <- typeCheckExpr sym a
  ta `mustBe` TNum
  tb <- typeCheckExpr sym b
  tb `mustBe` TNum
  pure TNum

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
  when (isOpaque ta)
    $ throw $ TypeIsOpaque ta
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
  tys <- traverse (typeCheckExpr sym) us
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
      TRec FRec <$> M.traverseWithKey throwIfDup ((M.unionWith checkDup `on` (Just <$>)) as bs)
    _ -> throw NeedRecordTypesForUnion
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
    Nothing -> TFun t <$> typeCheckExpr (M.insert i (t, Init) sym) e

typeCheckExpr sym (App f a) = do
  tf <- typeCheckExpr sym f
  ta <- typeCheckExpr sym a
  case tf of
    TFun i o -> ta `mustBe` i $> o
    _ -> throw $ ExpectedFunFound tf

typeCheckExpr sym (Deref e) = typeCheckExpr sym e >>= unwrapTRef

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

typeCheckStmt sym (Print e) = do
  t <- typeCheckExpr sym e
  when (isOpaque t)
    $ throw $ TypeIsOpaque t
  pure sym

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

typeCheckStmt sym (Open expr) = do
  tf <- typeCheckExpr sym expr
  tf `mustBe` TStr
  pure sym

typeCheckStmt sym (Read ident t expr) = do
  tf <- typeCheckExpr sym expr
  tf `mustBe` TStr
  (typ, _) <- lookupVar ident sym
  typ `mustBe` t
  when (isOpaque typ)
    $ throw $ TypeIsOpaque typ
  pure $ M.insert ident (typ, Init) sym

typeCheckStmt sym (Close expr) = do
  tf <- typeCheckExpr sym expr
  tf `mustBe` TStr
  pure sym

typeCheckStmt sym (New i e) = do
  t <- typeCheckExpr sym e
  (typ, _) <- lookupVar i sym
  typ `mustBe` TRef t
  pure $ M.insert i (typ, Init) sym

typeCheckStmt sym (WriteAt lhs rhs) = do
  tl <- unwrapTRef =<< typeCheckExpr sym lhs
  tr <- typeCheckExpr sym rhs
  tr `mustBe` tl
  pure sym

typeCheck :: Program -> TLI Program
typeCheck prog = toTLI $ prog <$ typeCheckStmt M.empty prog
