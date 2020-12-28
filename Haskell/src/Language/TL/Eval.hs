-- If we got to this point, we know typechecking succeeded, so we can use incomplete patterns
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module Language.TL.Eval(Val(..), EvalState(..), mkEvalState, eval) where

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M

import Control.Monad.Except(runExceptT, ExceptT, throwError, MonadError)
import Control.Monad.State (MonadState, gets, runStateT, modify, get)

import Language.TL.Syntax
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State.Lazy (StateT)
import Data.IORef (writeIORef, newIORef, readIORef, IORef)

data Val
  = VInt Integer
  | VBool Bool
  | VStr String
  | forall f. VRec (Field f) (Map f Val)
  | VFun Env Ident Expr
  | VRef (IORef Val)

instance Show Val where
  show (VInt i) = show i
  show (VBool b) = show b
  show (VStr s) = show s
  show (VRec f m) = showFields False f " = " m
  show VFun{} = "<λ>"
  show VRef{}= "<🗲>"

instance Ord Val where
  compare (VInt a) (VInt b) = compare a b
  compare (VBool a) (VBool b) = compare a b
  compare (VStr a) (VStr b) = compare a b
  compare (VRec FRec a) (VRec FRec b) = compare a b
  compare (VRec FTup a) (VRec FTup b) = compare a b
  compare _ _ = error "Opaque values in comparison. Did you run the typechecker?"

instance Eq Val where
  (==) = ((== EQ) .) . compare

unit :: Val
unit = VRec FTup M.empty

valType :: Val -> Type
valType (VInt _) = TInt
valType (VBool _) = TBool
valType (VStr _) = TStr
valType (VRec f m) = TRec f (valType <$> m)
valType _ = error "Opaque value in valType. Did you run the typechecker?"

type Env = Map Ident Val
type Files = Map String [Val]

data EvalState =
  EvalState
  { fs :: Files
  , open :: Files
  , env :: Env
  }

mkEvalState :: Files -> EvalState
mkEvalState fs =
  EvalState
  { fs
  , open = M.empty
  , env = M.empty
  }

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
      go (FileDoesNotExist f) = "The file " ++ show f ++ " does not exist in the filesystem"
      go (FileAlreadyOpened f) = "The file " ++ show f ++ " has already been opened"
      go (FileAlreadyClosed f) = "The file " ++ show f ++ " has been closed"
      go (FileNotOpened f) = "The file " ++ show f ++ " has not been opened"

      go (ReadDifferentType f found expected) =
        "In file " ++ show f
        ++ ", found a value of type " ++ show found
        ++ ", but expected a value of type " ++ show expected

      go (ReachedEOF f) = "There are no more values to read in file " ++ show f

withEnv :: MonadState EvalState m => Env -> m a -> m a
withEnv newEnv m = do
  oldEnv <- gets env
  modify \p -> p { env = newEnv }
  a <- m
  modify \p -> p { env = oldEnv }
  pure a

evalExpr :: (MonadState EvalState m, MonadError EvalError m, MonadIO m) => Expr -> m Val
evalExpr (IntLit i) = pure $ VInt i
evalExpr (BoolLit b) = pure $ VBool b
evalExpr (StrLit s) = pure $ VStr s
evalExpr (Var ident) = gets $ (M.! ident) . env

evalExpr (Arith a op b) = do
  a' <- evalExpr a
  b' <- evalExpr b
  case (b', op) of
    (VStr vb, Add) ->
      case a' of
        VStr va -> pure $ VStr $ va ++ vb
    (VInt 0, Divide) -> throwError DivisionByZero
    (VInt 0, Remainder) -> throwError DivisionByZero
    (VInt vb, _) ->
      case a' of
        VInt va -> pure $ VInt $ arithOp op va vb

evalExpr (Logic a op b) = do
  a' <- evalExpr a
  case (a', op) of
    (VBool True, Or) -> pure a'
    (VBool False, And) -> pure a'
    (VBool va, _) -> do
      b' <- evalExpr b
      case b' of
        VBool vb -> pure $ VBool $ logicOp op va vb

evalExpr (Comp a op b) = do
  a' <- evalExpr a
  b' <- evalExpr b
  pure $ VBool $ compOp op a' b'

evalExpr (RecLit f m) = VRec f <$> traverse evalExpr m

evalExpr (RecMember lhs f i) = do
  v <- evalExpr lhs
  case (v, f) of
    (VRec FRec m, FRec) -> pure $ m M.! i
    (VRec FTup m, FTup) -> pure $ m M.! i

evalExpr (RecUnion a b) = do
  ra <- evalExpr a
  rb <- evalExpr b
  case (ra, rb) of
    (VRec FRec a', VRec FRec b') -> pure $ VRec FRec $ M.union b' a'

evalExpr (Lam i _ e) = gets $ (\env -> VFun env i e) . env

evalExpr (App f a) = do
  vf <- evalExpr f
  va <- evalExpr a
  case vf of
    VFun env' i e -> withEnv (M.insert i va env') (evalExpr e)

evalExpr (Deref e) = do
  v <- evalExpr e
  case v of
    VRef ioRef -> liftIO $ readIORef ioRef

evalExpr (Print expr) = do
  v <- evalExpr expr
  liftIO $ print v
  pure unit

evalExpr (If cond then' else') = do
  c' <- evalExpr cond
  case c' of
    VBool c -> do
      env <- gets env
      withEnv env (evalExpr if c then then' else else')

evalExpr (Open expr) = do
  EvalState{..} <- get
  vf <- evalExpr expr
  case vf of
    VStr f ->
      case M.lookup f fs of
        Nothing -> throwError $ FileDoesNotExist f
        Just content ->
          case M.lookup f open of
            Just _ -> throwError $ FileAlreadyOpened f
            Nothing -> unit <$ modify \p -> p { open = M.insert f content open }

evalExpr (Read t expr) = do
  EvalState{..} <- get
  vf <- evalExpr expr
  case vf of
    VStr f ->
      case M.lookup f open of
        Nothing -> throwError $ FileNotOpened f
        Just [] -> throwError $ ReachedEOF f
        Just (c : cs) ->
          let tc = valType c
          in if tc /= t
            then throwError $ ReadDifferentType f tc t
            else c <$ modify \p -> p { open = M.insert f cs open }

evalExpr (Close expr) = do
  EvalState{..} <- get
  vf <- evalExpr expr
  case vf of
    VStr f ->
      case M.lookup f open of
        Nothing -> throwError $ FileAlreadyClosed f
        Just _ -> unit <$ modify \p -> p { open = M.delete f open }

evalExpr (New e) = do
  v <- evalExpr e
  VRef <$> liftIO (newIORef v)

evalExpr (WriteAt lhs rhs) = do
  vl <- evalExpr lhs
  vr <- evalExpr rhs
  case vl of
    VRef ioRef -> unit <$ liftIO (writeIORef ioRef vr)

evalExpr (Let i _ v e) = do
  v' <- evalExpr v
  modify \p -> p { env = M.insert i v' (env p) }
  evalExpr e

evalExpr (Seq a b) = evalExpr a *> evalExpr b

runEval :: StateT EvalState (ExceptT EvalError IO) a -> EvalState -> IO ()
runEval m s = runExceptT (runStateT m s) >>= \case
  Left e -> print e
  Right _ -> pure ()

eval :: EvalState -> Expr -> IO ()
eval s e = runEval (evalExpr e) s
