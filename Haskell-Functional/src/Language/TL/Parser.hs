module Language.TL.Parser where

import Data.Bifunctor(first)
import Data.Functor((<&>), ($>))
import Data.List(nub, foldl')
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Control.Monad.Except(liftEither, MonadError)
import Text.Parsec hiding (parse)

import Language.TL.Syntax

type Parser = Parsec String ()

comma, colon, equals, shebang, multiLine, singleLine, comment, ws :: Parser ()
comma = ws <* char ',' <* ws
colon = ws <* char ':' <* ws
equals = ws <* char '=' <* ws
shebang = try $ string "#!" *> manyTill anyChar (endOfLine $> ()) $> ()
multiLine = try $ string "{-" *> manyTill (multiLine <|> (anyChar $> ())) (try $ string "-}") $> ()
singleLine = try $ string "--" *> manyTill anyChar (eof <|> endOfLine $> ()) $> ()
comment = singleLine <|> multiLine
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
    term = (,) <$> (idx <* ws <* sep <* ws) <*> (rhs <* ws)

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
  , string "File" $> TFile
  ]

tupToRec :: [a] -> Map Int a
tupToRec = M.fromList . zip [0..]

ttup :: Parser Type
ttup = tuple (TRec FTup . tupToRec) type'

trec :: Parser Type
trec = TRec FRec <$> record '{' ident colon type'

tref :: Parser Type
tref = char '&' *> ws *> typeNoFun <&> TRef

typeNoFun :: Parser Type
typeNoFun = choice [try tref, try trec, try ttup, primType]

type' :: Parser Type
type' = chainr1 typeNoFun $ try (ws *> string "->" *> ws $> TFun)

number :: (Read a, Num a) => Parser a
number = read <$> many1 digit

intRaw :: Parser Integer
intRaw = sign <*> number
  where
    sign = option id (char '-' $> negate)

boolRaw :: Parser Bool
boolRaw = choice [string "True" $> True, string "False" $> False]

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

int :: Parser Expr
int = IntLit <$> intRaw

bool :: Parser Expr
bool = BoolLit <$> boolRaw

str :: Parser Expr
str = StrLit <$> strRaw

simpleLit :: Parser Expr
simpleLit = choice [try str, try int, bool]

reserved :: [String]
reserved = ["if", "and", "or", "let", "in", "open", "read", "close", "new", "from", "print", "then", "else"]

ident :: Parser String
ident = notReserved =<< (:) <$> fstChar <*> many sndChar
  where
    fstChar = lower
    sndChar = choice [letter, digit, char '\'']
    notReserved ((`elem` reserved) -> True) = fail "Reserved identifier"
    notReserved i = pure i

var :: Parser Expr
var = Var <$> ident

member :: Parser Expr -> Parser Expr
member lhs = foldl' unroll <$> lhs <*> many (char '.' *> (Left <$> number <|> Right <$> ident) <* ws)
  where
    unroll lhs' (Left idx) = RecMember lhs' FTup idx
    unroll lhs' (Right ident') = RecMember lhs' FRec ident'

vtup :: Parser Expr
vtup = tuple (RecLit FTup . tupToRec) exprFull

vrec :: Parser Expr
vrec = RecLit FRec <$> record '{' ident equals expr

if' :: Parser Expr
if' =
  If
  <$> (string "if" *> ws *> expr <* ws)
  <*> (string "then" *> ws *> expr <* ws)
  <*> (string "else" *> ws *> expr <* ws)

opMul :: Parser (Expr -> Expr -> Expr)
opMul =
  choice
  [ char '*' $> flip Arith Multiply
  , char '/' $> flip Arith Divide
  , char '%' $> flip Arith Remainder
  , char '&' $> RecUnion
  ]
  <* ws

opAdd :: Parser (Expr -> Expr -> Expr)
opAdd =
  choice
  [ char '+' $> flip Arith Add
  , char '-' $> flip Arith Subtract
  ]
  <* ws

opComp :: Parser (Expr -> Expr -> Expr)
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

opLogic :: Parser (Expr -> Expr -> Expr)
opLogic =
  choice
  [ string "and" $> flip Logic And
  , string "or" $> flip Logic Or
  ]
  <* ws

lam :: Parser Expr
lam = mkLam <$> (many1 param <* string "->" <* ws) <*> exprNoSeq
  where
    param = parens '(' ')' $ (,) <$> (ident <* colon) <*> type'
    mkLam [] e = e
    mkLam ((i, t) : as) e = Lam i t $ mkLam as e

deref :: Parser Expr
deref = char '!' *> ws *> exprNoOps <&> Deref

exprNoMember :: Parser Expr
exprNoMember = choice (try <$> [new, read', open, close, print', if', deref, lam, vrec, vtup, simpleLit, var]) <* ws

exprNoOps :: Parser Expr
exprNoOps = try (member exprNoMember) <|> exprNoMember

termMul :: Parser Expr
termMul = chainl1 exprNoOps (ws $> App)

termAdd :: Parser Expr
termAdd = chainl1 termMul opMul

termComp :: Parser Expr
termComp = chainl1 termAdd opAdd

termLogic :: Parser Expr
termLogic = chainl1 termComp opComp

expr :: Parser Expr
expr = chainr1 termLogic opLogic

print' :: Parser Expr
print' = Print <$> (string "print" <* ws *> expr)

open :: Parser Expr
open = Open <$> (string "open" *> ws *> expr)

read' :: Parser Expr
read' = Read <$> (string "read" *> ws *> type' <* ws) <*> (string "from" *> ws *> expr)

close :: Parser Expr
close = string "close" *> ws *> expr <&> Close

new :: Parser Expr
new = string "new" *> ws *> expr <&> New

writeAt :: Parser Expr
writeAt = WriteAt <$> expr <*> (string ":=" *> ws *> expr)

let' :: Parser Expr
let' =
  Let
  <$> (string "let" *> ws *> ident)
  <*> option Nothing (try $ colon *> type' <&> Just)
  <*> (equals *> expr)
  <*> (ws *> string "in" *> ws *> exprFull)

exprNoSeq :: Parser Expr
exprNoSeq = try let' <|> try writeAt <|> expr

exprFull :: Parser Expr
exprFull = exprNoSeq `chainr1` (char ';' *> ws $> Seq)

program :: Parser Expr
program = option () shebang *> ws *> exprFull <* eof

parse :: MonadError String m => String -> m Expr
parse = liftEither . first show . runParser program () ""
