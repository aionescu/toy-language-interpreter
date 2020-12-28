module Language.TL.Opts(Cmd(..), Opts(..), getOpts) where

import Options.Generic

data Cmd
  = Run
  | DumpAst { noTypeCheck :: Bool }
  deriving stock Generic

instance ParseRecord Cmd where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

data Opts = Opts Cmd String

instance ParseRecord Opts where
  parseRecord = Opts <$> parseRecord <*> parseRecord

getOpts :: IO Opts
getOpts = getRecord "Toy λanguage Interpreter"
