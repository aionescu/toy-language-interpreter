module Language.TL.Syntax where

import Data.List(intercalate)
import Data.Bifunctor(first)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M

type Ident = String

data Field :: * -> * where
  FRec :: Field Ident
  FTup :: Field Int

deriving instance Eq (Field f)
deriving instance Show (Field f)

data Type
  = TInt
  | TBool
  | TStr
  | forall f. TRec (Field f) (Map f Type)
  | TFun Type Type
  | TRef Type

instance Eq Type where
  TInt == TInt = True
  TBool == TBool = True
  TStr == TStr = True
  TRec FRec a == TRec FRec b = a == b
  TRec FTup a == TRec FTup b = a == b
  TFun a b == TFun a' b' = a == a' && b == b'
  TRef a == TRef b = a == b
  _ == _ = False

isOpaque :: Type -> Bool
isOpaque (TRef _) = True
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
  show (TRef t) = "&" ++ show t

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

data Expr
  = IntLit Integer
  | BoolLit Bool
  | StrLit String
  | Var Ident
  | Arith Expr ArithOp Expr
  | Logic Expr LogicOp Expr
  | Comp Expr CompOp Expr
  | forall f. Show f => RecLit (Field f) (Map f Expr)
  | forall f. Show f => RecMember Expr (Field f) f
  | RecUnion Expr Expr
  | Lam Ident Type Expr
  | App Expr Expr
  | Deref Expr
  | Print Expr
  | If Expr Expr Expr
  | Open Expr
  | Read Type Expr
  | Close Expr
  | New Expr
  | WriteAt Expr Expr
  | Seq Expr Expr
  | Let Ident (Maybe Type) Expr Expr

deriving instance Show Expr

type Program = Expr

type TLI a = Either String a

throw :: e -> Either e a
throw = Left

toTLI :: Show e => Either e a -> TLI a
toTLI = first show

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)
