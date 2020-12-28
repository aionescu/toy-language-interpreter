{-# LANGUAGE UndecidableInstances #-}

module Language.TL.TypeCk(typeCheck) where

import Data.Functor(($>))
import Control.Monad(when)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Control.Monad.Reader(ReaderT, runReaderT, local, ask, MonadReader)
import Control.Monad.Except(throwError, MonadError)

import Language.TL.Syntax

type TypeEnv = Map Ident Type

unit :: Type
unit = TRec FTup M.empty

data TypeError
  = ExpectedFound Type Type
  | UndeclaredVar Ident
  | ExpectedRecFound Type
  | forall f. NoFieldInRec Type (Field f) f
  | NeedRecordTypesForUnion
  | ExpectedFunFound Type
  | TypeIsOpaque Type
  | CanOnlyAppendStrings
  | CanOnlyAddIntegers
  | ExpectedRefFound Type

instance Show TypeError where
  show te = "Type error: " ++ go te ++ "."
    where
      go (ExpectedFound expected found) = "Expected " ++ show expected ++ ", but found " ++ show found
      go (UndeclaredVar ident) = "Variable " ++ ident ++ " was not declared"
      go (ExpectedRecFound t@(TRec FRec _)) = "Expected tuple type, but found record type " ++ show t
      go (ExpectedRecFound t@(TRec FTup _)) = "Expected record type, but found tuple type " ++ show t
      go (ExpectedRecFound t) = "Expected tuple or record type, but found " ++ show t
      go (NoFieldInRec t FRec i) = "The record type " ++ show t ++ " has no field named " ++ i
      go (NoFieldInRec t FTup i) = "The tuple type " ++ show t ++ " does not have enough elements to be indexed by the index " ++ show i
      go NeedRecordTypesForUnion = "Both operands of the \"&\" operator must be of record types"
      go (ExpectedFunFound t) = "Expected function type, but found " ++ show t
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

typeCheckExpr :: (MonadReader TypeEnv m, MonadError TypeError m) => Expr -> m Type
typeCheckExpr (IntLit _) = pure TInt
typeCheckExpr (BoolLit _) = pure TBool
typeCheckExpr (StrLit _) = pure TStr

typeCheckExpr (Var ident) = ask >>= lookupVar ident

typeCheckExpr (Arith a Add b) = do
  ta <- typeCheckExpr a
  tb <- typeCheckExpr b
  case (ta, tb) of
    (TInt, TInt) -> pure TInt
    (TStr, TStr) -> pure TStr
    _ ->
      throwError
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

  when (isOpaque ta) $
    throwError $ TypeIsOpaque ta

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

typeCheckExpr (RecUnion a b) = do
  ta <- typeCheckExpr a
  tb <- typeCheckExpr b
  case (ta, tb) of
    (TRec FRec as, TRec FRec bs) -> pure $ TRec FRec $ M.union bs as
    _ -> throwError NeedRecordTypesForUnion

typeCheckExpr (Lam i t e) = TFun t <$> local (M.insert i t) (typeCheckExpr e)

typeCheckExpr (App f a) = do
  tf <- typeCheckExpr f
  ta <- typeCheckExpr a
  case tf of
    TFun i o -> ta `mustBe` i $> o
    _ -> throwError $ ExpectedFunFound tf

typeCheckExpr (Deref e) = typeCheckExpr e >>= unwrapTRef

typeCheckExpr (Print e) = do
  t <- typeCheckExpr e

  when (isOpaque t) $
    throwError $ TypeIsOpaque t

  pure unit

typeCheckExpr (If cond then' else') = do
  tc <- typeCheckExpr cond
  tc `mustBe` TBool

  tThen <- typeCheckExpr then'
  tElse <- typeCheckExpr else'

  tElse `mustBe` tThen
  pure tThen

typeCheckExpr (Open expr) = do
  tf <- typeCheckExpr expr
  tf `mustBe` TStr
  pure TFile

typeCheckExpr (Read t expr) = do
  when (isOpaque t) $
    throwError $ TypeIsOpaque t

  tf <- typeCheckExpr expr
  tf `mustBe` TFile

  pure t

typeCheckExpr (Close expr) = do
  tf <- typeCheckExpr expr
  tf `mustBe` TFile
  pure unit

typeCheckExpr (New e) = do
  t <- typeCheckExpr e
  pure $ TRef t

typeCheckExpr (WriteAt lhs rhs) = do
  tl <- unwrapTRef =<< typeCheckExpr lhs
  tr <- typeCheckExpr rhs

  tr `mustBe` tl
  pure unit

typeCheckExpr (Seq a b) = do
  ta <- typeCheckExpr a
  ta `mustBe` unit

  typeCheckExpr b

typeCheckExpr (Let i t v e) = do
  tv <- typeCheckExpr v

  case t of
    Nothing -> pure ()
    Just t' -> tv `mustBe` t'

  local (M.insert i tv) $
    typeCheckExpr e

runTC :: ReaderT TypeEnv (Either TypeError) a -> TLI ()
runTC m = toTLI $ runReaderT m M.empty $> ()

typeCheck :: Program -> TLI Program
typeCheck prog = runTC (typeCheckExpr prog >>= (`mustBe` unit)) $> prog
