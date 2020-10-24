module Main where

import Data.Function((&))

import Language.TL.Parser
import Language.TL.TypeCk
import Language.TL.Eval
import Language.TL.Opts

run :: Opts -> IO ()
run (Opts Run{..} path) = do
  code <- readFile path

  parse code
    >>= typeCheck
    >>= (if smallStep then (showSteps <$>) . allSteps else (showOut <$>) . eval)
    & either id id
    & putStrLn

run (Opts DumpAst{..} path) = do
  code <- readFile path

  parse code
    >>= (if noTypeCheck then pure else typeCheck)
    & either id show
    & putStrLn

main :: IO ()
main = getOpts >>= run
