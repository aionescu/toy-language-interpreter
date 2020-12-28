module Main where

import Data.Function((&))

import Language.TL.Parser(parse)
import Language.TL.TypeCk
import Language.TL.Eval
import Language.TL.FS
import Language.TL.Opts

getCode :: String -> IO String
getCode "-" = getContents
getCode path = readFile path

run :: Opts -> IO ()
run (Opts Run{..} path) = do
  fs <- loadFS fsRoot
  code <- getCode path

  code
    & parse
    >>= typeCheck
    & either putStrLn (eval $ mkEvalState fs)

run (Opts DumpAst{..} path) = do
  code <- getCode path

  code
    & parse
    >>= (if noTypeCheck then pure else typeCheck)
    & either id show
    & putStrLn

main :: IO ()
main = getOpts >>= run
