module Language.Vanillalog.Generic.CLI.Util where

import Protolude

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.String (fromString)

import           Language.Vanillalog.Generic.AST

succeedOrDie :: (BS.ByteString -> Either [ Char ] b)
             -> BS.ByteString
             -> (b -> IO a)
             -> IO a
succeedOrDie processor bs action = do
  case processor bs of
    Right ast -> action ast
    Left err -> panic . fromString $ err
