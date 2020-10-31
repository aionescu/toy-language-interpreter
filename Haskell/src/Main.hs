module Main where

import Data.Function((&))

import Language.TL.Parser
import Language.TL.TypeCk
import Language.TL.Eval
import Language.TL.Opts

getCode :: String -> IO String
getCode "-" = getContents
getCode path = readFile path

run :: Opts -> IO ()
run (Opts Run{..} path) = do
  code <- getCode path

  parse code
    >>= typeCheck
    >>=
      (if smallStep
        then pure . traverseSteps_ putStrLn
        else (putStrLn . showOut <$>) . finalState)
    & either putStrLn id

run (Opts DumpAst{..} path) = do
  code <- getCode path

  parse code
    >>= (if noTypeCheck then pure else typeCheck)
    & either id show
    & putStrLn

main :: IO ()
main = getOpts >>= run
