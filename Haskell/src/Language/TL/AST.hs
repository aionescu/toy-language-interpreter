module Language.TL.AST where

import Data.List(intercalate)
import Data.Bifunctor(first)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M

type Ident = String

data Field :: * -> * where
  FRec :: Field Ident
  FTup :: Field Int

deriving instance Eq (Field f)

data Type
  = TInt
  | TBool
  | TStr
  | forall f. TRec (Field f) (Map f Type)
  | TFun Type Type

instance Eq Type where
  TInt == TInt = True
  TBool == TBool = True
  TStr == TStr = True
  TRec FRec a == TRec FRec b = a == b
  TRec FTup a == TRec FTup b = a == b
  TFun a b == TFun a' b' = a == a' && b == b'
  _ == _ = False

isOpaque :: Type -> Bool
isOpaque (TFun _ _) = True
isOpaque (TRec _ m) = any isOpaque m
isOpaque _ = False

withParens :: String -> String -> [String] -> String
withParens begin end l = begin ++ intercalate ", " l ++ end

showF :: Field f -> f -> String
showF FRec ident = ident
showF FTup idx = show idx

showField :: Field f -> String -> (f, String) -> String
showField f sep (i, s) = showF f i ++ sep ++ s

showFields :: Show v => Bool -> Field f -> String -> Map f v -> String
showFields False FRec sep m = withParens "{ " " }" (showField FRec sep <$> M.toList (show <$> m))
showFields True FRec sep m = withParens " | " " }" (showField FRec sep <$> M.toList (show <$> m))
showFields True FTup sep m = withParens " | " " }" (showField FTup sep <$> M.toList (show <$> m))
showFields False FTup _ m =
  case snd <$> M.toList m of
    [a] -> "(" ++ show a ++ ",)"
    l -> withParens "(" ")" (show <$> l)

instance Show Type where
  show TInt = "Int"
  show TBool = "Bool"
  show TStr = "Str"
  show (TRec f m) = showFields False f ": " m
  show (TFun a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

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
  show Eq = "=="
  show NEq = "!="

compOp :: Ord a => CompOp -> a -> a -> Bool
compOp Gt = (>)
compOp GtEq = (>=)
compOp Lt = (<)
compOp LtEq = (<=)
compOp Eq = (==)
compOp NEq = (/=)

-- `Expr 'L` are lvalue-expressions, `Expr 'R` are rvalue-expression
data ExprKind = L | R

data Expr :: ExprKind -> * where
  Default :: Type -> Expr 'R
  IntLit :: Integer -> Expr 'R
  BoolLit :: Bool -> Expr 'R
  StrLit :: String -> Expr 'R
  Var :: Ident -> Expr a
  Arith :: Expr a -> ArithOp -> Expr b -> Expr 'R
  Logic :: Expr a -> LogicOp -> Expr b -> Expr 'R
  Comp :: Expr a -> CompOp -> Expr b -> Expr 'R
  RecLit :: Field f -> Map f (Expr a) -> Expr 'R
  RecMember :: Expr a -> Field f -> f -> Expr a
  RecWith :: Expr a -> Field f -> Map f (Expr b) -> Expr 'R
  RecUnion :: Expr a -> Expr b -> Expr 'R
  Lam :: Ident -> Type -> Expr a -> Expr 'R
  App :: Expr a -> Expr b -> Expr 'R

instance Show (Expr a) where
  show (Default t) = "default " ++ show t
  show (IntLit i) = show i
  show (BoolLit b) = show b
  show (StrLit s) = show s
  show (Var ident) = ident
  show (Arith a op b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"
  show (Logic a op b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"
  show (Comp a op b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"
  show (RecLit f m) = showFields False f " = " m
  show (RecMember e f i) = show e ++ "." ++ showF f i
  show (RecWith lhs f updates) = "{ " ++ show lhs ++ showFields True f " = " updates
  show (RecUnion a b) = "(" ++ show a ++ " & " ++ show b ++ ")"
  show (Lam i t e) = "((" ++ i ++ ": " ++ show t ++ ") -> " ++ show e ++ ")"
  show (App f a) = "(" ++ show f ++ " " ++ show a ++ ")"

data Stmt
  = Nop
  | Decl Ident Type
  | Assign (Expr 'L) (Expr 'R)
  | DeclAssign Ident (Maybe Type) (Expr 'R)
  | Print (Expr 'R)
  | If (Expr 'R) Stmt Stmt
  | While (Expr 'R) Stmt
  | Compound Stmt Stmt
  | Open (Expr 'R)
  | Read Ident Type (Expr 'R)
  | Close (Expr 'R)

instance Show Stmt where
  show Nop = "nop"
  show (Decl ident type') = "let " ++ ident ++ ": " ++ show type'
  show (Assign ident expr) = show ident ++ " = " ++ show expr
  show (DeclAssign ident Nothing expr) = "let " ++ ident ++ " = " ++ show expr
  show (DeclAssign ident (Just type') expr) = "let " ++ ident ++ ": " ++ show type' ++ " = " ++ show expr
  show (Print expr) = "print " ++ show expr
  show (If cond then' Nop) = "if " ++ show cond ++ " { " ++ show then' ++ " }"
  show (If cond then' else') =
    "if " ++ show cond ++ " { " ++ show then' ++ " } else { " ++ show else' ++ " }"
  show (While cond body) = "while " ++ show cond ++ " { " ++ show body ++ " }"
  show (Compound a b) = show a ++ "; " ++ show b
  show (Open f) = "open " ++ show f
  show (Read i t f) = "read " ++ i ++ ": " ++ show t ++ " = " ++ show f
  show (Close f) = "close " ++ show f

type Program = Stmt

type TLI a = Either String a

throw :: e -> Either e a
throw = Left

toTLI :: Show e => Either e a -> TLI a
toTLI = first show
