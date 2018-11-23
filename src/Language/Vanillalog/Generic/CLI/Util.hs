module Language.Vanillalog.Generic.CLI.Util where

import Protolude

import System.Exit (exitFailure)

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.String (fromString)
import qualified Data.Text as T


import           Language.Vanillalog.Generic.AST
import qualified Language.Vanillalog.Generic.Logger as L
import           Language.Vanillalog.Generic.Pretty (pp)

succeedOrDie :: (BS.ByteString -> L.LoggerM b)
             -> BS.ByteString
             -> (b -> IO a)
             -> IO a
succeedOrDie processor bs action = do
  mResult <- L.runLoggerT $ processor bs
  case mResult of
    Just result -> action result
    Nothing     -> exitFailure
