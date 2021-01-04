module Main where

import Data.Function((&))

import Language.TL.Eval(eval)
import Language.TL.Opts(Opts(..), Cmd(..), getOpts)
import Language.TL.Parser(parse)
import Language.TL.TypeCk(typeCheck)

getCode :: String -> IO String
getCode "-" = getContents
getCode path = readFile path

run :: Opts -> IO ()
run (Opts Run path) = do
  code <- getCode path

  code
    & parse
    >>= typeCheck
    & either putStrLn eval

run (Opts DumpAst{..} path) = do
  code <- getCode path

  code
    & parse
    >>= (if noTypeCheck then pure else typeCheck)
    & either id show
    & putStrLn

main :: IO ()
main = getOpts >>= run