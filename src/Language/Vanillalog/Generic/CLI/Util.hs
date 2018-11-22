module Language.Vanillalog.Generic.CLI.Util where

import Protolude

import System.Exit (exitFailure)

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.String (fromString)
import qualified Data.Text as T


import Language.Vanillalog.Generic.AST
import Language.Vanillalog.Generic.Error (Error)
import Language.Vanillalog.Generic.Pretty (pp)

succeedOrDie :: (BS.ByteString -> Either [ Error ] b)
             -> BS.ByteString
             -> (b -> IO a)
             -> IO a
succeedOrDie processor bs action =
  case processor bs of
    Right ast -> action ast
    Left errs -> do
      let prettyErrs = map pp errs
      traverse_ putStrLn prettyErrs
      exitFailure
