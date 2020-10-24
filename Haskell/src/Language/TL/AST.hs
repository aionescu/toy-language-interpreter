module Language.TL.AST where

import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Map.Strict(Map)

type Ident = String

data Field :: * -> * where
  FRec :: Ident -> Field Ident
  FTup :: Int -> Field Int

deriving instance Eq (Field a)
deriving instance Ord (Field a)

instance Show (Field a) where
  show (FRec ident) = ident
  show (FTup idx) = show idx

data Fields :: * -> * -> * where
  FsRec :: Map (Field Ident) a -> Fields Ident a
  FsTup :: Map (Field Int) a -> Fields Int a

data Type :: * where
  TInt :: Type
  TBool :: Type
  TRec :: Fields a Type -> Type

instance Eq Type where
  TInt == TInt = True
  TBool == TBool = True
  TRec (FsRec as) == TRec (FsRec bs) = as == bs
  TRec (FsTup as) == TRec (FsTup bs) = as == bs
  _ == _ = False

withParens :: String -> String -> [String] -> String
withParens begin end l = begin ++ intercalate ", " l ++ end

showField :: String -> (Field a, String) -> String
showField sep (f, s) = show f ++ " " ++ sep ++ " " ++ s

showFields :: Show v => Bool -> String -> Fields a v -> String
showFields False sep (FsRec m) = withParens "{ " " }" (showField sep <$> M.toList (show <$> m))
showFields True sep (FsRec m) = withParens " | " " }" (showField sep <$> M.toList (show <$> m))
showFields True sep (FsTup m) = withParens " | " " }" (showField sep <$> M.toList (show <$> m))
showFields False _ (FsTup m) =
  case snd <$> M.toList m of
    [a] -> "(" ++ show a ++ ",)"
    l -> withParens "(" ")" (show <$> l)

instance Show Type where
  show TInt = "Int"
  show TBool = "Bool"
  show (TRec fs) = showFields False ":" fs

data Val :: * where
  VInt :: Int -> Val
  VBool :: Bool -> Val
  VRec :: Fields a Val -> Val

instance Show Val where
  show (VBool b) = show b
  show (VInt i) = show i
  show (VRec fs) = showFields False "<-" fs

data ArithOp
  = Add
  | Subtract
  | Multiply
  | Divide
  | Remainder

instance Show ArithOp where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"
  show Remainder = "%"

arithOp :: Integral a => ArithOp -> a -> a -> a
arithOp Add = (+)
arithOp Subtract = (-)
arithOp Multiply = (*)
arithOp Divide = quot
arithOp Remainder = rem

data LogicOp
  = And
  | Or

instance Show LogicOp where
  show And = "and"
  show Or = "or"

logicOp :: LogicOp -> Bool -> Bool -> Bool
logicOp And = (&&)
logicOp Or = (||)

data CompOp
  = Gt
  | GtEq
  | Lt
  | LtEq
  | Eq
  | NEq

instance Show CompOp where
  show Gt = ">"
  show GtEq = ">="
  show Lt = "<"
  show LtEq = "<="
  show Eq = "="
  show NEq = "<>"

compOp :: Ord a => CompOp -> a -> a -> Bool
compOp Gt = (>)
compOp GtEq = (>=)
compOp Lt = (<)
compOp LtEq = (<=)
compOp Eq = (==)
compOp NEq = (/=)

-- `Expr L` are lvalue-expressions, `Expr R` are rvalue-expression
data ExprKind = L | R

data Expr :: ExprKind -> * where
  Lit :: Val -> Expr R
  Var :: Ident -> Expr a
  Arith :: Expr a -> ArithOp -> Expr b -> Expr R
  Logic :: Expr a -> LogicOp -> Expr b -> Expr R
  Comp :: Expr a -> CompOp -> Expr b -> Expr R
  RecLit :: Fields f (Expr a) -> Expr R
  RecMember :: Expr a -> Field f -> Expr a
  RecWith :: Expr a -> Fields f ({- forall b. -} Expr b) -> Expr R -- No ImpredicativePolymorphism yet
  RecUnion :: Expr a -> Expr b -> Expr R

instance Show (Expr a) where
  show (Lit v) = show v
  show (Var ident) = ident
  show (Arith a op b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"
  show (Logic a op b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"
  show (Comp a op b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"
  show (RecLit fs) = showFields False "<-" fs
  show (RecMember e f) = show e ++ "." ++ show f
  show (RecWith lhs updates) = "{ " ++ show lhs ++ showFields True "<-" updates
  show (RecUnion a b) = show a ++ " | " ++ show b

data Stmt
  = Nop
  | Decl Ident Type
  | Assign (Expr L) (Expr R)
  | DeclAssign Ident (Maybe Type) (Expr R)
  | Print (Expr R)
  | If (Expr R) Stmt Stmt
  | While (Expr R) Stmt
  | Compound Stmt Stmt

instance Show Stmt where
  show Nop = ""
  show (Decl ident type') = ident ++ " : " ++ show type'
  show (Assign ident expr) = show ident ++ " <- " ++ show expr
  show (DeclAssign ident Nothing expr) = ident ++ " : _ <- " ++ show expr
  show (DeclAssign ident (Just type') expr) = ident ++ " : " ++ show type' ++ " <- " ++ show expr
  show (Print expr) = "print " ++ show expr
  show (If cond then' Nop) = "if " ++ show cond ++ " { " ++ show then' ++ " }"
  show (If cond then' else') =
    "if " ++ show cond ++ " { " ++ show then' ++ " } else {" ++ show else' ++ " }"
  show (While cond body) = "while " ++ show cond ++ " { " ++ show body ++ " }"
  show (Compound a b) = show a ++ "; " ++ show b

type Program = Stmt

type TLI a = Either String a

throw :: a -> Either a b
throw = Left

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right b) = Right b

toTLI :: Show a => Either a b -> TLI b
toTLI = mapLeft show

traverseM :: Applicative f => (a -> f b) -> Map k a -> f (Map k b)
traverseM = M.traverseWithKey . const
