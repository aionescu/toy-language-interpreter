{-# LANGUAGE UndecidableInstances #-}

module Language.TL.TypeCk(typeCheck) where

import Data.Function(on)
import Data.Functor(($>))
import Control.Monad(when)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Control.Monad.Reader(ReaderT, runReaderT, local, ask, MonadReader)
import Control.Monad.State (StateT, runStateT, modify, put, get, MonadState)
import Control.Monad.Except(throwError, MonadError)

import Language.TL.AST

data VarState = Init | Uninit
  deriving Eq

type VarInfo = (Type, VarState)
type TypeEnv = Map Ident VarInfo

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
  | CanOnlyAddIntegers
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
      go CanOnlyAddIntegers = "Both operands of the addition operation must be integers"
      go (ExpectedRefFound t) = "Expected reference type, but found type " ++ show t

unwrapTRef :: MonadError TypeError f => Type -> f Type
unwrapTRef (TRef t) = pure t
unwrapTRef t = throwError $ ExpectedRefFound t

mustBe :: MonadError TypeError f => Type -> Type -> f ()
mustBe found expected
  | found == expected = pure ()
  | otherwise = throwError $ ExpectedFound expected found

lookupVar :: MonadError TypeError m => Ident -> Map Ident a -> m a
lookupVar var sym = maybe (throwError $ UndeclaredVar var) pure $ M.lookup var sym

isolate :: MonadState s m => m a -> m (a, s)
isolate m = do
  s <- get
  a <- m
  s' <- get
  put s
  pure (a, s')

typeCheckExpr :: (MonadReader TypeEnv m, MonadError TypeError m) => Expr a -> m Type
typeCheckExpr (Default t) = do
  when (isOpaque t)
    $ throwError $ TypeIsOpaque t
  pure t

typeCheckExpr (IntLit _) = pure TInt
typeCheckExpr (BoolLit _) = pure TBool
typeCheckExpr (StrLit _) = pure TStr

typeCheckExpr (Var ident) = do
  (type', state) <- lookupVar ident =<< ask
  case state of
    Uninit -> throwError $ UninitializedVar ident
    Init -> pure type'

typeCheckExpr (Arith a Add b) = do
  ta <- typeCheckExpr a
  tb <- typeCheckExpr b
  case (ta, tb) of
    (TInt, TInt) -> pure TInt
    (TStr, TStr) -> pure TStr
    _ -> throwError $
      if TStr `elem` [ta, tb]
        then CanOnlyAppendStrings
        else CanOnlyAddIntegers

typeCheckExpr (Arith a _ b) = do
  ta <- typeCheckExpr a
  ta `mustBe` TInt
  tb <- typeCheckExpr b
  tb `mustBe` TInt
  pure TInt

typeCheckExpr (Logic a _ b) = do
  ta <- typeCheckExpr a
  ta `mustBe` TBool
  tb <- typeCheckExpr b
  tb `mustBe` TBool
  pure TBool

typeCheckExpr (Comp a _ b) = do
  ta <- typeCheckExpr a
  tb <- typeCheckExpr b
  tb `mustBe` ta
  when (isOpaque ta)
    $ throwError $ TypeIsOpaque ta
  pure TBool

typeCheckExpr (RecLit f m) = TRec f <$> traverse typeCheckExpr m

typeCheckExpr (RecMember lhs f i) = do
  t <- typeCheckExpr lhs
  case (t, f) of
    (TRec FRec m, FRec) ->
      case M.lookup i m of
        Nothing -> throwError $ NoFieldInRec t f i
        Just t' -> pure t'
    (TRec FTup m, FTup) ->
      case M.lookup i m of
        Nothing -> throwError $ NoFieldInRec t f i
        Just t' -> pure t'
    _ -> throwError $ ExpectedRecFound t

typeCheckExpr (RecWith lhs f us) = do
  t <- typeCheckExpr lhs
  tys <- traverse typeCheckExpr us
  case (t, f) of
    (TRec FRec m, FRec) -> do
      M.traverseWithKey (checkMember t f m) tys $> t
    (TRec FTup m, FTup) -> do
      M.traverseWithKey (checkMember t f m) tys $> t
    _ -> throwError $ ExpectedRecFound t
  where
    checkMember :: (Ord f, MonadError TypeError m) => Type -> Field f -> Map f Type -> f -> Type -> m ()
    checkMember rec f' m i t =
      case M.lookup i m of
        Nothing -> throwError $ NoFieldInRec rec f' i
        Just t' -> t `mustBe` t'

typeCheckExpr (RecUnion a b) = do
  ta <- typeCheckExpr a
  tb <- typeCheckExpr b
  case (ta, tb) of
    (TRec FRec as, TRec FRec bs) -> do
      TRec FRec <$> M.traverseWithKey throwIfDup ((M.unionWith checkDup `on` (Just <$>)) as bs)
    _ -> throwError NeedRecordTypesForUnion
  where
    checkDup a' b'
      | a' == b' = a'
      | otherwise = Nothing

    throwIfDup :: MonadError TypeError m => Ident -> Maybe Type -> m Type
    throwIfDup ident Nothing = throwError $ DuplicateIncompatibleField ident
    throwIfDup _ (Just t) = pure t

typeCheckExpr (Lam i t e) = do
  sym <- ask
  case M.lookup i sym of
    Just _ -> throwError $ CantShadow i
    Nothing -> TFun t <$> local (M.insert i (t, Init)) (typeCheckExpr e)

typeCheckExpr (App f a) = do
  tf <- typeCheckExpr f
  ta <- typeCheckExpr a
  case tf of
    TFun i o -> ta `mustBe` i $> o
    _ -> throwError $ ExpectedFunFound tf

typeCheckExpr (Deref e) = typeCheckExpr e >>= unwrapTRef

mergeVarInfo :: VarInfo -> VarInfo -> VarInfo
mergeVarInfo a b
  | a == b = a
  | otherwise = (fst a, Uninit)

hoistReader :: MonadState s m => ReaderT s m a -> m a
hoistReader m = runReaderT m =<< get

typeCheckExpr' :: (MonadState TypeEnv m, MonadError TypeError m) => Expr a -> m Type
typeCheckExpr' = hoistReader . typeCheckExpr

typeCheckStmt :: (MonadState TypeEnv m, MonadError TypeError m) => Stmt -> m ()
typeCheckStmt Nop = pure ()

typeCheckStmt (Decl ident type') = do
  sym <- get
  case M.lookup ident sym of
    Just _ -> throwError $ VarAlreadyDeclared ident
    Nothing -> modify $ M.insert ident (type', Uninit)

typeCheckStmt (Assign (Var ident) expr) = do
  (typ, _) <- lookupVar ident =<< get
  typ2 <- typeCheckExpr' expr
  typ2 `mustBe` typ
  modify $ M.insert ident (typ, Init)

typeCheckStmt (Assign (RecMember lhs f i) expr) =
  typeCheckStmt $ Assign lhs $ RecWith lhs f $ M.singleton i expr

typeCheckStmt (DeclAssign ident (Just type') expr) = do
  typeCheckStmt (Decl ident type')
  typeCheckStmt (Assign (Var ident) expr)

typeCheckStmt (DeclAssign ident Nothing expr) = do
  t <- typeCheckExpr' expr
  typeCheckStmt $ DeclAssign ident (Just t) expr

typeCheckStmt (Print e) = do
  t <- typeCheckExpr' e
  when (isOpaque t)
    $ throwError $ TypeIsOpaque t

typeCheckStmt (If cond then' else') = do
  tc <- typeCheckExpr' cond
  tc `mustBe` TBool

  (_, thenTbl) <- isolate $ typeCheckStmt then'
  (_, elseTbl) <- isolate $ typeCheckStmt else'

  modify (M.intersectionWith mergeVarInfo thenTbl elseTbl `M.intersection`)

typeCheckStmt (While cond body) = do
  tc <- typeCheckExpr' cond
  tc `mustBe` TBool
  isolate (typeCheckStmt body) $> ()

typeCheckStmt (Compound a b) = do
  typeCheckStmt a
  typeCheckStmt b

typeCheckStmt (Open expr) = do
  tf <- typeCheckExpr' expr
  tf `mustBe` TStr

typeCheckStmt (Read ident t expr) = do
  tf <- typeCheckExpr' expr
  tf `mustBe` TStr
  (typ, _) <- lookupVar ident =<< get
  typ `mustBe` t
  when (isOpaque typ)
    $ throwError $ TypeIsOpaque typ
  modify $ M.insert ident (typ, Init)

typeCheckStmt (Close expr) = do
  tf <- typeCheckExpr' expr
  tf `mustBe` TStr

typeCheckStmt (New i e) = do
  t <- typeCheckExpr' e
  (typ, _) <- lookupVar i =<< get
  typ `mustBe` TRef t
  modify $ M.insert i (typ, Init)

typeCheckStmt (WriteAt lhs rhs) = do
  tl <- unwrapTRef =<< typeCheckExpr' lhs
  tr <- typeCheckExpr' rhs
  tr `mustBe` tl

runTC :: StateT TypeEnv (Either TypeError) a -> TLI ()
runTC m = toTLI $ runStateT m M.empty $> ()

typeCheck :: Program -> TLI Program
typeCheck prog = runTC (typeCheckStmt prog) $> prog
