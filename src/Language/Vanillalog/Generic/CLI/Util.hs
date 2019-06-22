{-# LANGUAGE DataKinds #-}

module Language.Vanillalog.Generic.CLI.Util where

import Protolude

import System.Exit (exitFailure)

import Text.PrettyPrint hiding ((<>))

import           Data.Text (pack)
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Language.Exalog.Core as E
import qualified Language.Exalog.Logger as L
import           Language.Exalog.Pretty (pp)
import           Language.Exalog.Pretty.Helper (prettyC)
import qualified Language.Exalog.Relation as R
import qualified Language.Exalog.Tuples as T

import           Language.Vanillalog.AST
import qualified Language.Vanillalog.Generic.AST as AG

succeedOrDie :: (BS.ByteString -> L.Logger b)
             -> BS.ByteString
             -> (b -> IO a)
             -> IO a
succeedOrDie processor bs action = do
  mResult <- L.runLoggerT $ processor bs
  case mResult of
    Just result -> action result
    Nothing     -> exitFailure

display :: Program -> R.Solution 'E.ABase -> IO ()
display program sol =
  forM_ (zip [(1 :: Int)..] (AG.queries program)) $ \(ix, query) -> do
    putStrLn . pp $ query
    R.findTuplesByPredSym (E.PredicateSymbol $ "query_" <> (pack . show) ix) sol $
      putStrLn . displayTuples
    putStrLn ("" :: Text)
  where
  displayTuples :: T.Tuples n -> Text
  displayTuples tuples
    | T.isEmpty tuples = "There are no answers to this query."
    | otherwise        = pack . render . vcat . prettyC $ tuples
