module AST where

import Data.List (intercalate)

type Ident = String

data Type
  = TInt
  | TBool
  | TTup [Type]
  deriving stock Eq

withParens :: String -> String -> [String] -> String
withParens begin end l = begin ++ intercalate ", " l ++ end

instance Show Type where
  show TInt = "Int"
  show TBool = "Bool"
  show (TTup [t]) = "(" ++ show t ++ ",)"
  show (TTup ts) = withParens "(" ")" (show <$> ts)

data Val
  = VBool Bool
  | VInt Int
  | VTup [Val]

instance Show Val where
  show (VBool b) = show b
  show (VInt i) = show i
  show (VTup [v]) = "(" ++ show v ++ ",)"
  show (VTup vs) = withParens "(" ")" (show <$> vs)

data ArithOp
  = Add
  | Subtract
  | Multiply
  | Divide
  | Remainder
  deriving Eq

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
  Lit :: Val -> Expr 'R
  Var :: Ident -> Expr a
  Arith :: Expr a -> ArithOp -> Expr b -> Expr 'R
  Logic :: Expr a -> LogicOp -> Expr b -> Expr 'R
  Comp :: Expr a -> CompOp -> Expr b -> Expr 'R
  TupLit :: [Expr a] -> Expr 'R
  TupMember :: Expr a -> Int -> Expr a
  With :: Expr a -> [(Int, Expr b)] -> Expr 'R

instance Show (Expr a) where
  show (Lit v) = show v
  show (Var ident) = ident
  show (Arith a op b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"
  show (Logic a op b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"
  show (Comp a op b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"
  show (TupLit es) = withParens "(" ")" (show <$> es)
  show (TupMember e i) = show e ++ "." ++ show i
  show (With lhs updates) = show lhs ++ " " ++ withParens "{ " " }" (showUpdate <$> updates)
    where
      showUpdate (idx, expr) = show idx ++ " <- " ++ show expr

data Stmt
  = Nop
  | Decl Ident Type
  | Assign (Expr 'L) (Expr 'R)
  | DeclAssign Ident (Maybe Type) (Expr 'R)
  | Print (Expr 'R)
  | If (Expr 'R) Stmt Stmt
  | While (Expr 'R) Stmt
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
