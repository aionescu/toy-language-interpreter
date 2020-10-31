module Language.TL.Opts(Cmd(..), Opts(..), getOpts) where

import Control.Applicative(liftA2)
import Options.Generic

data Cmd
  = Run { smallStep :: Bool }
  | DumpAst { noTypeCheck :: Bool }
  deriving stock Generic

instance ParseRecord Cmd where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

data Opts = Opts Cmd String

instance ParseRecord Opts where
  parseRecord = liftA2 Opts parseRecord parseRecord

getOpts :: IO Opts
getOpts = getRecord "Toy Language Interpreter"
