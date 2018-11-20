module Language.Vanillalog.Generic.CLI.Util where

import Protolude

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.String (fromString)
import qualified Data.Text as T

import           Language.Vanillalog.Generic.AST

succeedOrDie :: (BS.ByteString -> Either [ Text ] b)
             -> BS.ByteString
             -> (b -> IO a)
             -> IO a
succeedOrDie processor bs action = do
  case processor bs of
    Right ast -> action ast
    Left errs -> panic $ T.intercalate "\n" errs
