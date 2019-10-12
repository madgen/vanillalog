{-# LANGUAGE DataKinds #-}

module Language.Vanillalog.Generic.CLI.Util where

import Protolude

import System.Exit (exitFailure)

import Text.PrettyPrint hiding ((<>))

import           Data.Text (pack)

import qualified Language.Exalog.Core as E
import           Language.Exalog.Pretty (pp)
import           Language.Exalog.Pretty.Helper (Pretty, prettyC)
import qualified Language.Exalog.Relation as R
import qualified Language.Exalog.Tuples as T

import           Language.Vanillalog.AST
import qualified Language.Vanillalog.Generic.AST as AG
import qualified Language.Vanillalog.Generic.Stage as S
import           Language.Vanillalog.Generic.Pretty (HasPrecedence)

succeedOrDie :: S.StageEnv -> S.Stage a -> IO a
succeedOrDie env processor = maybe exitFailure pure =<< S.runStage env processor

display :: Pretty (hop 'Nullary) => Pretty (hop 'Unary) => Pretty (hop 'Binary)
        => Pretty (bop 'Nullary) => Pretty (bop 'Unary) => Pretty (bop 'Binary)
        => HasPrecedence hop => HasPrecedence bop
        => AG.Program decl hop bop -> R.Solution 'E.ABase -> IO ()
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
