module Language.TL.Syntax where

import Data.List(intercalate)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M

type Ident = String

data Type
  = Num
  | Bool
  | Str
  | File
  | Rec (Map Ident Type)
  | Tup [Type]
  | Type :-> Type
  | Ref Type
  deriving stock Eq

unit :: Type
unit = Tup []

intrinsicTypes :: Map Ident Type
intrinsicTypes =
  M.fromList
  [ ("Num", Num)
  , ("Bool", Bool)
  , ("Str", Str)
  , ("File", File)
  ]

showField :: Show a => String -> String -> a -> String
showField sep i a = i ++ sep ++ show a

instance Show Type where
  show Num = "Num"
  show Bool = "Bool"
  show Str = "Str"
  show File = "File"
  show (Rec m) | M.null m = "{ }"
  show (Rec m) = "{ " ++ intercalate ", " (uncurry (showField ": ") <$> M.toList m) ++ " }"
  show (Tup [t]) = "(" ++ show t ++ ",)"
  show (Tup ts) = "(" ++ intercalate ", " (show <$> ts) ++ ")"
  show (a@(_ :-> _) :-> b) = "(" ++ show a ++ ") -> " ++ show b
  show (a :-> b) = show a ++ " -> " ++ show b
  show (Ref t@(_ :-> _)) = "&(" ++ show t ++ ")"
  show (Ref t) = "&" ++ show t

data TyExpr
  = TyVar Ident
  | TyIntersect TyExpr TyExpr
  | TyFn TyExpr TyExpr
  | TyRec [(Ident, TyExpr)]
  | TyTup [TyExpr]
  | TyRef TyExpr
  deriving stock Show

tyUnit :: TyExpr
tyUnit = TyTup []

data ArithOp
  = Add
  | Subtract
  | Multiply
  | Divide
  | Remainder
  deriving stock Show

arithOp :: Integral a => ArithOp -> a -> a -> a
arithOp Add = (+)
arithOp Subtract = (-)
arithOp Multiply = (*)
arithOp Divide = quot
arithOp Remainder = rem

data LogicOp
  = And
  | Or
  deriving stock Show

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
  deriving stock Show

compOp :: Ord a => CompOp -> a -> a -> Bool
compOp Gt = (>)
compOp GtEq = (>=)
compOp Lt = (<)
compOp LtEq = (<=)
compOp Eq = (==)
compOp NEq = (/=)

data Expr t
  = NumLit Integer
  | BoolLit Bool
  | StrLit String
  | Var Ident
  | Arith (Expr t) ArithOp (Expr t)
  | Logic (Expr t) LogicOp (Expr t)
  | Comp (Expr t) CompOp (Expr t)
  | RecLit [(Ident, Expr t)]
  | TupLit [Expr t]
  | RecMember (Expr t) Ident
  | TupMember (Expr t) Int
  | Intersect (Expr t) (Expr t)
  | Lam Ident t (Expr t)
  | App (Expr t) (Expr t)
  | Deref (Expr t)
  | Print (Expr t)
  | If (Expr t) (Expr t) (Expr t)
  | Show (Expr t)
  | Open (Expr t)
  | Read t (Expr t)
  | Close (Expr t)
  | NewRef (Expr t)
  | RefAssign (Expr t) (Expr t)
  | Seq (Expr t) (Expr t)
  | Let Bool Ident (Maybe t) (Expr t) (Expr t)
  | LetTy Ident t (Expr t)
  | As (Expr t) t
  deriving stock Show
