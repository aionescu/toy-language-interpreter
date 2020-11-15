module Language.TL.FS(loadFS) where

import System.Directory(getDirectoryContents)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Text.Parsec hiding (parse)

import Language.TL.AST
import Language.TL.Parser
import Language.TL.Eval
import System.Exit (exitSuccess)
import System.FilePath (combine)

vNum :: Parser Val
vNum = VNum <$> intRaw

vBool :: Parser Val
vBool = VBool <$> boolRaw

vStr :: Parser Val
vStr = VStr <$> strRaw

vRec :: Parser Val
vRec = VRec FRec <$> record '{' ident equals val

vTup :: Parser Val
vTup = tuple (VRec FTup . tupToRec) val

val :: Parser Val
val = choice [try vRec, try vTup, try vStr, try vBool, vNum]

line :: Parser Val
line = ws *> val <* eof

parseLine :: String -> TLI Val
parseLine = toTLI . runParser line () ""

processFile :: String -> TLI [Val]
processFile s = traverse parseLine $ lines s

loadFS :: Maybe String -> IO (Map String [Val])
loadFS Nothing = pure M.empty
loadFS (Just dir) = do
  fs <- filter (not . (`elem` [".", ".."])) <$> getDirectoryContents dir
  contents <- traverse readFile $ combine dir <$> fs

  let contents' = traverse processFile contents

  case contents' of
    Left e -> putStrLn e *> exitSuccess
    Right cs -> pure $ M.fromList $ zip fs cs
