module Language.TL.Parser(parse) where

import Data.Functor(($>))
import Control.Applicative(liftA2)
import Text.Parsec hiding (parse)
import qualified Data.Map.Strict as M
import Data.Map.Strict(Map)

import Language.TL.AST
import Data.List(nub, foldl')

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

parens :: Char -> Char -> Parser a -> Parser a
parens begin end = between (char begin *> ws) (char end *> ws)

tuple :: ([a] -> a) -> Parser a -> Parser a
tuple ctor term = parens '(' ')' $ uncurry mkTup <$> elems
  where
    withTrailing = (, True) <$> many (term <* ws <* comma)
    noTrailing = (, False) <$> sepBy1 term comma
    elems = try withTrailing <|> noTrailing

    mkTup [a] False = a
    mkTup l _ = ctor l

record :: Parser sep -> Parser a -> Parser (Fields Ident a)
record sep rhs = FsRec . M.fromList <$> (unique =<< parens '{' '}' elems)
  where
    withTrailing = many (term <* comma)
    noTrailing = sepBy1 term comma
    elems = try withTrailing <|> noTrailing
    term = liftA2 (,) (FRec <$> ident <* ws <* sep <* ws) (rhs <* ws)

    unique es =
      let es' = fst <$> es
      in
        if nub es' == es'
          then pure es
          else fail "Fields in a record must be unique"

primType :: Parser Type
primType = choice [string "Int" $> TInt, string "Bool" $> TBool]

tupToRec :: [a] -> Fields Int a
tupToRec = FsTup . M.fromList . zip (FTup <$> [0..])

ttup :: Parser Type
ttup = tuple (TRec . tupToRec) type'

trec :: Parser Type
trec = TRec <$> record colon type'

type' :: Parser Type
type' = try trec <|> try ttup <|> primType

member :: Parser (Expr a) -> Parser (Expr a)
member lhs = liftA2 (foldl' unroll) lhs (many $ char '.' *> (Left <$> number <|> Right <$> ident) <* ws)
  where
    unroll lhs' (Left idx) = RecMember lhs' (FTup idx)
    unroll lhs' (Right ident') = RecMember lhs' (FRec ident')

vtup :: Parser (Expr R)
vtup = tuple (RecLit . tupToRec) expr

vrec :: Parser (Expr R)
vrec = RecLit <$> record arrow expr

lvalue :: Parser (Expr L)
lvalue = try (member var) <|> var

comma :: Parser ()
comma = char ',' *> ws

opMul :: Parser (Expr R -> Expr R -> Expr R)
opMul =
  choice
  [ char '*' $> flip Arith Multiply
  , char '/' $> flip Arith Divide
  , char '%' $> flip Arith Remainder
  , char '|' $> RecUnion
  ]
  <* ws

opAdd :: Parser (Expr R -> Expr R -> Expr R)
opAdd =
  choice
  [ char '+' $> flip Arith Add
  , char '-' $> flip Arith Subtract
  ]
  <* ws

opComp :: Parser (Expr R -> Expr R -> Expr R)
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

opLogic :: Parser (Expr R -> Expr R -> Expr R)
opLogic =
  choice
  [ string "and" $> flip Logic And
  , string "or" $> flip Logic Or
  ]
  <* ws

withBlock :: Ord a => Parser a -> Parser (Map a (Expr R))
withBlock index = M.fromList <$> (unique =<< parens '{' '}' (sepBy update comma))
  where
    update = liftA2 (,) (index <* arrow) (expr <* ws)
    unique es =
      let es' = fst <$> es
      in
        if nub es' == es'
          then pure es
          else fail "Updates in a with-expression must be unique"

exprNoMember :: Parser (Expr R)
exprNoMember = choice [try vrec, try vtup, Lit <$> try val, var] <* ws

exprNoWith :: Parser (Expr R)
exprNoWith = try (member exprNoMember) <|> exprNoMember

withExpr :: Ord a => (Expr R -> Map a (Expr R) -> Expr R) -> Parser a -> Parser (Expr R)
withExpr ctor index = liftA2 ctor (exprNoWith <* ws <* string "with" <* ws) (withBlock index)

exprNoOps :: Parser (Expr R)
exprNoOps = try withRecord <|> try withTup <|> exprNoWith
  where
    withRecord =  withExpr (\a -> RecWith a . FsRec) (FRec <$> ident)
    withTup = withExpr (\a -> RecWith a . FsTup) (FTup <$> number)

termAdd :: Parser (Expr R)
termAdd = chainl1 exprNoOps opMul

termComp :: Parser (Expr R)
termComp = chainl1 termAdd opAdd

termLogic :: Parser (Expr R)
termLogic = chainl1 termComp opComp

expr :: Parser (Expr R)
expr = chainr1 termLogic opLogic

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
declAssign = DeclAssign <$> (ident <* colon) <*> ((char '_' $> Nothing <|> Just <$> type') <* arrow) <*> expr

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
