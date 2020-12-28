-- If we got to this point, we know typechecking succeeded, so we can use incomplete patterns
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module Language.TL.Eval(eval) where

import Control.Monad.Except(liftEither, runExceptT, ExceptT, throwError, MonadError)
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Reader(asks, MonadReader(local), ReaderT(runReaderT))
import Control.Monad.State(MonadState, runStateT, modify, get)
import Control.Monad.State.Lazy(StateT)
import Data.Bifunctor(Bifunctor(first))
import Data.IORef(writeIORef, newIORef, readIORef, IORef)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Text.Parsec

import Language.TL.Parser hiding (expr, parse)
import Language.TL.Syntax

data Val
  = VInt Integer
  | VBool Bool
  | VStr String
  | forall f. VRec (Field f) (Map f Val)
  | VFun Env Ident Expr
  | VRef (IORef Val)
  | VFile String

instance Show Val where
  show (VInt i) = show i
  show (VBool b) = show b
  show (VStr s) = show s
  show (VRec f m) = showFields False f " = " m
  show _ = error "Opaque value in show. Did you run the typechecker?"

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

vInt :: Parser Val
vInt = VInt <$> intRaw

vBool :: Parser Val
vBool = VBool <$> boolRaw

vStr :: Parser Val
vStr = VStr <$> strRaw

vRec :: Parser Val
vRec = VRec FRec <$> record '{' ident equals val

vTup :: Parser Val
vTup = tuple (VRec FTup . tupToRec) val

val :: Parser Val
val = choice [try vRec, try vTup, try vStr, try vBool, vInt]

line :: Parser Val
line = ws *> val <* eof

parseLine :: MonadError EvalError m => FilePath -> String -> m Val
parseLine f = liftEither . first (InvalidFileFormat f . show) . runParser line () f

parseFile :: MonadError EvalError m => FilePath -> String -> m [Val]
parseFile f = traverse (parseLine f) . lines

openFile :: (MonadState Files m, MonadError EvalError m, MonadIO m) => FilePath -> m Val
openFile f = do
  cs <- parseFile f =<< liftIO (readFile f)
  VFile f <$ modify (M.insert f cs)

type Env = Map Ident Val
type Files = Map String [Val]

data EvalError
  = DivisionByZero
  | FileDoesNotExist String
  | FileAlreadyOpened String
  | FileAlreadyClosed String
  | FileNotOpened String
  | InvalidFileFormat String String
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
      go (InvalidFileFormat f err) = "The file " ++ show f ++ " is not a valid Toy Language input file. Parser error: \n" ++ err

      go (ReadDifferentType f found expected) =
        "In file " ++ show f
        ++ ", found a value of type " ++ show found
        ++ ", but expected a value of type " ++ show expected

      go (ReachedEOF f) = "There are no more values to read in file " ++ show f

evalExpr :: (MonadReader Env m, MonadState Files m, MonadError EvalError m, MonadIO m) => Expr -> m Val
evalExpr (IntLit i) = pure $ VInt i
evalExpr (BoolLit b) = pure $ VBool b
evalExpr (StrLit s) = pure $ VStr s
evalExpr (Var i) = asks (M.! i)

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

evalExpr (Lam i _ e) = asks \env -> VFun env i e

evalExpr (App f a) = do
  vf <- evalExpr f
  va <- evalExpr a
  case vf of
    VFun env' i e ->
      local (const $ M.insert i va env') $
        evalExpr e

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
    VBool c -> evalExpr if c then then' else else'

evalExpr (Open expr) = do
  files <- get
  vf <- evalExpr expr
  case vf of
    VStr f ->
      case M.lookup f files of
        Just _ -> throwError $ FileAlreadyOpened f
        Nothing -> openFile f

evalExpr (Read t expr) = do
  files <- get
  vf <- evalExpr expr
  case vf of
    VFile f ->
      case M.lookup f files of
        Nothing -> throwError $ FileNotOpened f
        Just [] -> throwError $ ReachedEOF f
        Just (c : cs) ->
          let tc = valType c
          in
            if tc /= t
            then throwError $ ReadDifferentType f tc t
            else c <$ modify (M.insert f cs)

evalExpr (Close expr) = do
  files <- get
  vf <- evalExpr expr
  case vf of
    VFile f ->
      case M.lookup f files of
        Nothing -> throwError $ FileAlreadyClosed f
        Just _ -> unit <$ modify (M.delete f)

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
  local (M.insert i v') $
    evalExpr e

evalExpr (Seq a b) = evalExpr a *> evalExpr b

runEval :: ReaderT Env (StateT Files (ExceptT EvalError IO)) a -> IO ()
runEval m = runExceptT (runStateT (runReaderT m M.empty) M.empty) >>= \case
  Left e -> print e
  Right _ -> pure ()

eval :: Expr -> IO ()
eval = runEval . evalExpr
