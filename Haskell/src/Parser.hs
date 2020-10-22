module Parser(parse) where

import Data.Functor(($>))
import Control.Applicative(liftA2)
import Text.Parsec hiding (parse)

import AST
import Data.List (foldl')

type Parser = Parsec String ()

multiLine :: Parser ()
multiLine = try $ string "{-" *> manyTill (multiLine <|> (anyChar $> ())) (try $ string "-}") $> ()

singleLine :: Parser ()
singleLine = try $ string "--" *> manyTill anyChar newline $> ()

comment :: Parser ()
comment = singleLine <|> multiLine

ws :: Parser ()
ws = spaces *> skipMany (comment *> spaces)

number :: Parser Int
number = read <$> many1 digit

int :: Parser Val
int = VInt <$> (sign <*> number)
  where
    sign = option id (char '-' $> negate)

bool :: Parser Val
bool = VBool <$> choice [string "True" $> True, string "False" $> False]

val :: Parser Val
val = int <|> bool

ident :: Parser String
ident = liftA2 (:) fstChar (many sndChar)
  where
    fstChar = choice [letter, char '_']
    sndChar = choice [fstChar, digit, char '\'']

var :: Parser (Expr a)
var = Var <$> ident

tuple :: ([a] -> a) -> Parser a -> Parser a
tuple ctor term = parens $ uncurry mkTup <$> (liftA2 (,) (sepBy1 term comma) (option False (comma $> True)))
  where
    parens = between (char '(' *> ws) (char ')' *> ws)
    mkTup [a] False = a
    mkTup l _ = ctor l

primType :: Parser Type
primType = choice [string "Int" $> TInt, string "Bool" $> TBool]

ttup :: Parser Type
ttup = tuple TTup type'

type' :: Parser Type
type' = try ttup <|> primType

tupMember :: Parser (Expr a) -> Parser (Expr a)
tupMember lhs = liftA2 (foldl' TupleMember) lhs (many $ char '.' *> number)

vtup :: Parser (Expr 'R)
vtup = tuple TupLit expr

lvalue :: Parser (Expr 'L)
lvalue = try (tupMember var) <|> var

comma :: Parser ()
comma = char ',' *> ws

opMul :: Parser (Expr 'R -> Expr 'R -> Expr 'R)
opMul =
  choice
  [ char '*' $> flip Arith Multiply
  , char '/' $> flip Arith Divide
  , char '%' $> flip Arith Remainder
  ]
  <* ws

opAdd :: Parser (Expr 'R -> Expr 'R -> Expr 'R)
opAdd =
  choice
  [ char '+' $> flip Arith Add
  , char '-' $> flip Arith Subtract
  ]
  <* ws

opComp :: Parser (Expr 'R -> Expr 'R -> Expr 'R)
opComp =
  choice
  [ try $ string "<=" $> flip Comp LtEq
  , try $ string ">=" $> flip Comp GtEq
  , try $ string "<>" $> flip Comp NEq
  , char '<' $> flip Comp Lt
  , char '>' $> flip Comp Gt
  , char '=' $> flip Comp Eq
  ]
  <* ws

opLogic :: Parser (Expr 'R -> Expr 'R -> Expr 'R)
opLogic =
  choice
  [ string "and" $> flip Logic And
  , string "or" $> flip Logic Or
  ]
  <* ws

termMul :: Parser (Expr 'R)
termMul =
  let
    expr' = choice [try vtup, Lit <$> try val, var] <* ws
    expr'' = try (tupMember expr') <|> expr'
    with = liftA2 (,) (string "with" *> ws *> number <* ws <* char '=' <* ws) expr
    tryWith e Nothing = e
    tryWith e (Just (idx, w)) = With e idx w
  in
    liftA2 tryWith expr'' (option Nothing $ Just <$> with)

termAdd :: Parser (Expr 'R)
termAdd = chainl1 termMul opMul

termComp :: Parser (Expr 'R)
termComp = chainl1 termAdd opAdd

termLogic :: Parser (Expr 'R)
termLogic = chainl1 termComp opComp

expr :: Parser (Expr 'R)
expr = chainl1 termLogic opLogic

print' :: Parser Stmt
print' = Print <$> (string "print" <* ws *> expr)

colon :: Parser ()
colon = ws <* char ':' <* ws

arrow :: Parser ()
arrow = ws <* string "<-" <* ws

decl :: Parser Stmt
decl = liftA2 Decl (ident <* colon) type'

assign :: Parser Stmt
assign = liftA2 Assign (lvalue <* arrow) expr

declAssign :: Parser Stmt
declAssign = DeclAssign <$> (ident <* colon) <*> (type' <* arrow) <*> expr

block :: Parser Stmt
block =
  char '{' *> ws
  *> stmt
  <* ws <* char '}' <* ws

if' :: Parser Stmt
if' = If <$> cond <*> block <*> option Nop elseBlock
  where
    cond = string "if" *> ws *> expr <* ws
    elseBlock = string "else" *> ws *> block

while :: Parser Stmt
while = While <$> cond <*> block
  where
    cond = string "while" *> ws *> expr <* ws

stmt' :: Parser Stmt
stmt' = choice [try while, try if', try declAssign, try assign, try decl, print'] <* ws

stmt :: Parser Stmt
stmt = option Nop $ stmt' `chainr1` (char ';' *> ws $> Compound)

program :: Parser Program
program = ws *> stmt <* eof

parse :: String -> TLI Program
parse = toTLI . runParser program () ""
