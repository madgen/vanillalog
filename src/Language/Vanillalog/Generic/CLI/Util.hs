module Language.Vanillalog.Generic.CLI.Util where

import Protolude

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.String (fromString)

import           Language.Vanillalog.Generic.AST

parseOrDie :: (BS.ByteString -> Either [ Char ] (Program b))
           -> BS.ByteString
           -> (Program b -> IO a)
           -> IO a
parseOrDie programParser bs action = do
  case programParser bs of
    Right ast -> action ast
    Left err -> panic . fromString $ err
