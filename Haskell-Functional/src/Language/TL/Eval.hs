module Language.TL.Eval(eval) where

import Control.Monad.Except(liftEither, runExceptT, ExceptT, throwError, MonadError)
import Control.Monad.Fix(MonadFix)
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Reader(asks, MonadReader(local), ReaderT(runReaderT))
import Control.Monad.State(MonadState, runStateT, modify, get)
import Control.Monad.State.Lazy(StateT)
import Data.Bifunctor(first)
import Data.Functor(($>))
import Data.IORef(writeIORef, newIORef, readIORef, IORef)
import Data.List(intercalate)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Text.Parsec

import Language.TL.Parser hiding (expr, parse)
import Language.TL.Syntax
import Language.TL.TypeChecker(isSubtypeOf)

panic :: String -> a
panic msg = error $ "Panicked on \"" ++ msg ++ "\"."

type Env = Map Ident Val
type Files = Map String [Val]

data Val
  = VNum Integer
  | VBool Bool
  | VStr String
  | VFile String
  | VRec (Map Ident Val)
  | VTup [Val]
  | VFn Env Ident (Expr Type)
  | VRef (IORef Val)

instance Show Val where
  show (VNum n) = show n
  show (VBool b) = show b
  show (VStr s) = show s
  show (VRec m) | M.null m = "{ }"
  show (VRec m) = "{ " ++ intercalate ", " (uncurry (showField " = ") <$> M.toList m) ++ " }"
  show (VTup [t]) = "(" ++ show t ++ ",)"
  show (VTup ts) = "(" ++ intercalate ", " (show <$> ts) ++ ")"
  show _ = panic "show opaque"

instance Ord Val where
  compare (VNum a) (VNum b) = compare a b
  compare (VBool a) (VBool b) = compare a b
  compare (VStr a) (VStr b) = compare a b
  compare (VRec a) (VRec b) = compare a b
  compare (VTup a) (VTup b) = compare a b
  compare _ _ = panic "compare opaque"

instance Eq Val where
  (==) = ((== EQ) .) . compare

valType :: Val -> Type
valType (VNum _) = Num
valType (VBool _) = Bool
valType (VStr _) = Str
valType (VRec m) = Rec (valType <$> m)
valType (VTup vs) = Tup (valType <$> vs)
valType _ = panic "valType opaque"

vInt :: Parser Val
vInt = VNum <$> intRaw

vBool :: Parser Val
vBool = VBool <$> boolRaw

vStr :: Parser Val
vStr = VStr <$> strRaw

vRec :: Parser Val
vRec = VRec . M.fromList <$> record (equals *> ws *> val)

vTup :: Parser Val
vTup = tuple VTup val

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
  modify (M.insert f cs) $> VFile f

data EvalError
  = DivisionByZero
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
      go (FileAlreadyOpened f) = "The file " ++ show f ++ " has already been opened"
      go (FileAlreadyClosed f) = "The file " ++ show f ++ " has been closed"
      go (FileNotOpened f) = "The file " ++ show f ++ " has not been opened"
      go (InvalidFileFormat f err) = "The file " ++ show f ++ " is not a valid Toy Language input file. Parser error: \n" ++ err

      go (ReadDifferentType f found expected) =
        "In file " ++ show f
        ++ ", found a value of type " ++ show found
        ++ ", but expected a value of a subtype of " ++ show expected

      go (ReachedEOF f) = "There are no more values to read in file " ++ show f

vUnit :: Val
vUnit = VTup []

upCast :: Type -> Val -> Val
upCast (Tup ts) (VTup vs) = VTup $ zipWith upCast ts vs
upCast (Rec ts) (VRec vs) = VRec $ M.intersectionWith upCast ts vs
upCast (_ :-> t) (VFn env i e) = VFn env i $ e `As` t
upCast _ v = v

eval' :: (MonadFix m, MonadReader Env m, MonadState Files m, MonadError EvalError m, MonadIO m) => Expr Type -> m Val
eval' (NumLit n) = pure $ VNum n
eval' (BoolLit b) = pure $ VBool b
eval' (StrLit s) = pure $ VStr s

eval' (Var i) = asks (M.! i)

eval' (Arith a op b) = do
  a' <- eval' a
  b' <- eval' b
  case (a', op, b') of
    (VStr va, Add, VStr vb) -> pure $ VStr $ va ++ vb
    (VNum _, Divide, VNum 0) -> throwError DivisionByZero
    (VNum _, Remainder, VNum 0) -> throwError DivisionByZero
    (VNum va, _, VNum vb) -> pure $ VNum $ arithOp op va vb
    _ -> panic "Arith"

eval' (Logic a op b) = do
  a' <- eval' a
  case (a', op) of
    (VBool True, Or) -> pure a'
    (VBool False, And) -> pure a'
    (VBool va, _) -> do
      b' <- eval' b
      case b' of
        VBool vb -> pure $ VBool $ logicOp op va vb
        _ -> panic "Logic RHS"
    _ -> panic "Logic"

eval' (Comp a op b) = do
  a' <- eval' a
  b' <- eval' b
  pure $ VBool $ compOp op a' b'

eval' (RecLit m) = VRec . M.fromList <$> traverse (traverse eval') m
eval' (TupLit es) = VTup <$> traverse eval' es

eval' (RecMember lhs i) = do
  v <- eval' lhs
  case v of
    VRec m -> pure $ m M.! i
    _ -> panic "RecMember"

eval' (TupMember lhs i) = do
  v <- eval' lhs
  case v of
    VTup vs ->  pure $ vs !! i
    _ -> panic "TupMember"

eval' (Intersect a b) = do
  ra <- eval' a
  rb <- eval' b
  case (ra, rb) of
    (VRec a', VRec b') -> pure $ VRec $ M.union b' a'
    _ -> panic "Intersect"

eval' (Lam i _ e) = asks \env -> VFn env i e

eval' (App f a) = do
  vf <- eval' f
  va <- eval' a
  case vf of
    VFn env' i e ->
      local (const $ M.insert i va env') $
        eval' e
    _ -> panic "App"

eval' (Deref e) = do
  v <- eval' e
  case v of
    VRef ioRef -> liftIO $ readIORef ioRef
    _ -> panic "Deref"

eval' (Print expr) = do
  v <- eval' expr
  liftIO $ print v
  pure vUnit

eval' (If cond then' else') = do
  c' <- eval' cond
  case c' of
    VBool c -> eval' if c then then' else else'
    _ -> panic "If"

eval' (Show e) = VStr . show <$> eval' e

eval' (Open expr) = do
  vf <- eval' expr
  files <- get
  case vf of
    VStr f ->
      case M.lookup f files of
        Just _ -> throwError $ FileAlreadyOpened f
        Nothing -> openFile f
    _ -> panic "Open"

eval' (Read t expr) = do
  vf <- eval' expr
  files <- get
  case vf of
    VFile f ->
      case M.lookup f files of
        Nothing -> throwError $ FileNotOpened f
        Just [] -> throwError $ ReachedEOF f
        Just (c : cs) ->
          let tc = valType c
          in
            if tc `isSubtypeOf` t
            then modify (M.insert f cs) $> upCast t c
            else throwError $ ReadDifferentType f tc t
    _ -> panic "Read"

eval' (Close expr) = do
  vf <- eval' expr
  files <- get
  case vf of
    VFile f ->
      case M.lookup f files of
        Nothing -> throwError $ FileAlreadyClosed f
        Just _ -> modify (M.delete f) $> vUnit
    _ -> panic "Close"

eval' (NewRef e) = do
  v <- eval' e
  VRef <$> liftIO (newIORef v)

eval' (RefAssign lhs rhs) = do
  vl <- eval' lhs
  vr <- eval' rhs
  case vl of
    VRef ioRef -> liftIO (writeIORef ioRef vr) $> vUnit
    _ -> panic "RefAssign"

eval' (Let False i _ v e) = do
  v' <- eval' v
  local (M.insert i v') $
    eval' e

eval' (Let True i _ v e) = mdo
  v' <- local (M.insert i v') $ eval' v

  local (M.insert i v') $
    eval' e

eval' (Seq a b) = eval' a *> eval' b

eval' LetTy{} = panic "LetTy"

eval' (e `As` t) = upCast t <$> eval' e

runEval :: ReaderT Env (StateT Files (ExceptT EvalError IO)) a -> IO ()
runEval m = runExceptT (runStateT (runReaderT m M.empty) M.empty) >>= \case
  Left e -> print e
  Right _ -> pure ()

eval :: Expr Type -> IO ()
eval = runEval . eval'
