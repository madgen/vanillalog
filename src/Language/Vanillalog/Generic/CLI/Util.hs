module Language.Vanillalog.Generic.CLI.Util where

import Protolude

import System.Exit (exitFailure)

import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Language.Exalog.Logger as L

succeedOrDie :: (BS.ByteString -> L.Logger b)
             -> BS.ByteString
             -> (b -> IO a)
             -> IO a
succeedOrDie processor bs action = do
  mResult <- L.runLoggerT $ processor bs
  case mResult of
    Just result -> action result
    Nothing     -> exitFailure
