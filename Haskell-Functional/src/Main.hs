module Main where

import Control.Category((>>>))
import Control.Monad((>=>))
import System.Environment(getArgs)

import Language.TL.Eval(eval)
import Language.TL.Parser(parse)
import Language.TL.TypeChecker(typeCheck)

getCode :: IO String
getCode = do
  [path] <- getArgs
  case path of
    "-" -> getContents
    _ -> readFile path

main :: IO ()
main = getCode >>= ((parse >=> typeCheck) >>> either putStrLn eval)
