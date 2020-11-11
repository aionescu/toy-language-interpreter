module Language.TL.Parser where

import Data.List(nub, foldl')
import Data.Functor((<&>), ($>))
import Control.Applicative(liftA2, liftA3)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Text.Parsec hiding (parse)

import Language.TL.AST

type Parser = Parsec String ()

comma :: Parser ()
comma = ws <* char ',' <* ws

colon :: Parser ()
colon = ws <* char ':' <* ws

equals :: Parser ()
equals = ws <* char '=' <* ws

shebang :: Parser ()
shebang = try $ string "#!" *> manyTill anyChar (endOfLine $> ()) $> ()

multiLine :: Parser ()
multiLine = try $ string "{-" *> manyTill (multiLine <|> (anyChar $> ())) (try $ string "-}") $> ()

singleLine :: Parser ()
singleLine = try $ string "--" *> manyTill anyChar (eof <|> endOfLine $> ()) $> ()

comment :: Parser ()
comment = singleLine <|> multiLine

ws :: Parser ()
ws = spaces *> skipMany (comment *> spaces)

parens :: Char -> Char -> Parser a -> Parser a
parens begin end = between (char begin *> ws) (char end *> ws)

tuple :: ([a] -> a) -> Parser a -> Parser a
tuple ctor term = parens '(' ')' $ uncurry mkTup <$> elems
  where
    withTrailing = (, True) <$> many (term <* comma)
    noTrailing = (, False) <$> sepBy1 term comma
    elems = try withTrailing <|> noTrailing

    mkTup [a] False = a
    mkTup l _ = ctor l

record :: Ord f => Char -> Parser f -> Parser sep -> Parser a -> Parser (Map f a)
record begin idx sep rhs = M.fromList <$> (unique =<< parens begin '}' elems)
  where
    withTrailing = many (term <* comma)
    noTrailing = sepBy1 term comma
    elems = try withTrailing <|> noTrailing
    term = liftA2 (,) (idx <* ws <* sep <* ws) (rhs <* ws)

    unique es =
      let es' = fst <$> es
      in
        if nub es' == es'
          then pure es
          else fail "Fields in a record must be unique"

primType :: Parser Type
primType = choice
  [ string "Int" $> TInt
  , string "Bool" $> TBool
  , string "Str" $> TStr
  ]

tupToRec :: [a] -> Map Int a
tupToRec = M.fromList . zip [0..]

ttup :: Parser Type
ttup = tuple (TRec FTup . tupToRec) type'

trec :: Parser Type
trec = TRec FRec <$> record '{' ident colon type'

typeNoFun :: Parser Type
typeNoFun = try trec <|> try ttup <|> primType

type' :: Parser Type
type' = chainr1 typeNoFun $ try (ws *> string "->" *> ws $> TFun)

number :: (Read a, Num a) => Parser a
number = read <$> many1 digit

intRaw :: Parser Integer
intRaw = sign <*> number
  where
    sign = option id (char '-' $> negate)

int :: Parser (Expr 'R)
int = IntLit <$> intRaw

boolRaw :: Parser Bool
boolRaw = choice [string "True" $> True, string "False" $> False]

bool :: Parser (Expr 'R)
bool = BoolLit <$> boolRaw

strRaw :: Parser String
strRaw = between quote quote $ many ch
  where
    unescape '\\' = '\\'
    unescape '"' = '"'
    unescape '0' = '\0'
    unescape 'n' = '\n'
    unescape 'r' = '\r'
    unescape 'v' = '\v'
    unescape 't' = '\t'
    unescape 'b' = '\b'
    unescape 'f' = '\f'
    unescape a = a

    escaped = char '\\' *> oneOf "\\\"0nrvtbf" <&> unescape
    regular = noneOf "\\\"\0\n\r\v\t\b\f"
    ch = regular <|> escaped
    quote = char '"'

str :: Parser (Expr 'R)
str = StrLit <$> strRaw

simpleLit :: Parser (Expr 'R)
simpleLit = choice [try str, try int, bool]

reserved :: [String]
reserved = ["if", "else", "while", "and", "or", "default", "let", "nop", "open", "read", "close"]

ident :: Parser String
ident = notReserved =<< liftA2 (:) fstChar (many sndChar)
  where
    fstChar = lower
    sndChar = choice [letter, digit, char '\'']
    notReserved ((`elem` reserved) -> True) = fail "Reserved identifier"
    notReserved i = pure i

var :: Parser (Expr a)
var = Var <$> ident

member :: Parser (Expr a) -> Parser (Expr a)
member lhs = liftA2 (foldl' unroll) lhs (many $ char '.' *> (Left <$> number <|> Right <$> ident) <* ws)
  where
    unroll lhs' (Left idx) = RecMember lhs' FTup idx
    unroll lhs' (Right ident') = RecMember lhs' FRec ident'

vtup :: Parser (Expr 'R)
vtup = tuple (RecLit FTup . tupToRec) expr

vrec :: Parser (Expr 'R)
vrec = RecLit FRec <$> record '{' ident equals expr

lvalue :: Parser (Expr 'L)
lvalue = try (member var) <|> var

opMul :: Parser (Expr 'R -> Expr 'R -> Expr 'R)
opMul =
  choice
  [ char '*' $> flip Arith Multiply
  , char '/' $> flip Arith Divide
  , char '%' $> flip Arith Remainder
  , char '&' $> RecUnion
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
  , try $ string "==" $> flip Comp Eq
  , try $ string "!=" $> flip Comp NEq
  , char '<' $> flip Comp Lt
  , char '>' $> flip Comp Gt
  ]
  <* ws

opLogic :: Parser (Expr 'R -> Expr 'R -> Expr 'R)
opLogic =
  choice
  [ string "and" $> flip Logic And
  , string "or" $> flip Logic Or
  ]
  <* ws

lam :: Parser (Expr 'R)
lam = liftA2 mkLam (many1 param <* string "->" <* ws) expr
  where
    param = parens '(' ')' $ liftA2 (,) (ident <* colon) type'
    mkLam [] e = e
    mkLam ((i, t) : as) e = Lam i t $ mkLam as e

default' :: Parser (Expr 'R)
default' = string "default" *> ws *> type' <&> Default

exprNoMember :: Parser (Expr 'R)
exprNoMember = choice [try default', try lam, try vrec, try vtup, try simpleLit, try var] <* ws

exprNoWith :: Parser (Expr 'R)
exprNoWith = try (member exprNoMember) <|> exprNoMember

withExpr :: Ord a => (Expr 'R -> Map a (Expr 'R) -> Expr 'R) -> Parser a -> Parser (Expr 'R)
withExpr ctor index = liftA2 ctor (char '{' *> ws *> exprNoWith <* ws) (record '|' index equals expr)

exprNoOps :: Parser (Expr 'R)
exprNoOps = try withRecord <|> try withTup <|> exprNoWith
  where
    withRecord = withExpr (`RecWith` FRec) ident
    withTup = withExpr (`RecWith` FTup) number

termMul :: Parser (Expr 'R)
termMul = chainl1 exprNoOps (ws $> App)

termAdd :: Parser (Expr 'R)
termAdd = chainl1 termMul opMul

termComp :: Parser (Expr 'R)
termComp = chainl1 termAdd opAdd

termLogic :: Parser (Expr 'R)
termLogic = chainl1 termComp opComp

expr :: Parser (Expr 'R)
expr = chainr1 termLogic opLogic

print' :: Parser Stmt
print' = Print <$> (string "print" <* ws *> expr)

decl :: Parser Stmt
decl = string "let" *> ws *> liftA2 Decl (ident <* colon) type'

assign :: Parser Stmt
assign = liftA2 Assign (lvalue <* equals) expr

declAssign :: Parser Stmt
declAssign =
  string "let" *> ws *> liftA3 DeclAssign ident (option Nothing $ Just <$> try (colon *> type')) (equals *> expr)

block :: Parser Stmt
block =
  char '{' *> ws
  *> stmt
  <* ws <* char '}' <* ws

if' :: Parser Stmt
if' = liftA3 If cond block (option Nop elseBlock)
  where
    cond = string "if" *> ws *> expr <* ws
    elseBlock = string "else" *> ws *> block

while :: Parser Stmt
while = liftA2 While cond block
  where
    cond = string "while" *> ws *> expr <* ws

nop :: Parser Stmt
nop = string "nop" $> Nop

open :: Parser Stmt
open = Open <$> (string "open" *> ws *> expr)

read' :: Parser Stmt
read' = liftA3 Read ident (colon *> type') (equals *> string "read" *> ws *> expr)

close :: Parser Stmt
close = string "close" *> ws *> expr <&> Close

stmt' :: Parser Stmt
stmt' = option Nop $ choice
  [ try while
  , try if'
  , try open
  , try read'
  , try close
  , try declAssign
  , try decl
  , try assign
  , try print'
  , nop
  ] <* ws

stmt :: Parser Stmt
stmt = stmt' `chainr1` (char ';' *> ws $> Compound)

program :: Parser Program
program = option () shebang *> ws *> stmt <* eof

parse :: String -> TLI Program
parse = toTLI . runParser program () ""
