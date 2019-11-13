{-# LANGUAGE DataKinds #-}

module Language.Vanillalog.Generic.CLI.Util
  ( succeedOrDie
  , display
  , displayTuples
  ) where

import Protolude hiding (pred)

import System.Exit (exitFailure)

import Text.PrettyPrint hiding ((<>))

import           Data.Text (pack)

import qualified Language.Exalog.Core as E
import           Language.Exalog.Pretty (pp)
import           Language.Exalog.Pretty.Helper (Pretty, prettyC, pretty)
import qualified Language.Exalog.Relation as R
import qualified Language.Exalog.Tuples as T
import qualified Language.Exalog.Logger as L
import           Language.Exalog.SrcLoc (span, SrcSpan)

import           Language.Vanillalog.AST
import qualified Language.Vanillalog.Generic.AST as AG
import qualified Language.Vanillalog.Generic.Stage as S
import           Language.Vanillalog.Generic.Pretty (HasPrecedence)

succeedOrDie :: S.StageEnv decl hop bop -> S.Stage decl hop bop a -> IO a
succeedOrDie env processor = maybe exitFailure pure =<< S.runStage env processor

display :: Pretty (hop 'Nullary) => Pretty (hop 'Unary) => Pretty (hop 'Binary)
        => Pretty (bop 'Nullary) => Pretty (bop 'Unary) => Pretty (bop 'Binary)
        => HasPrecedence hop => HasPrecedence bop
        => AG.Program decl hop bop -> R.Solution 'E.ABase -> IO ()
display program sol =
  forM_ (R.toList sol) $ \(R.Relation pred tuples) -> do
    -- Generated query heads contain the span of the overall query
    let querySpan = span pred
    mQuery <- findQueryM program querySpan

    case mQuery of
      Just query -> do
        putStrLn $ pp query
        putStrLn $ pack . render . displayTuples $ tuples
      Nothing -> pure ()

findQueryM :: AG.Program decl hop bop -> SrcSpan -> IO (Maybe (AG.Query hop bop))
findQueryM program querySpan = L.runLoggerT (L.LoggerEnv Nothing) $
  case findQuery program querySpan of
    Just query -> pure query
    Nothing    -> L.scream querySpan "This query cannot be found."

findQuery :: AG.Program decl hop bop -> SrcSpan -> Maybe (AG.Query hop bop)
findQuery pr s = find ((== s) . span) (AG.queries pr)

displayTuples :: T.Tuples a -> Doc
displayTuples tuples = nest 2 $
     "There are " <>  pretty nOfTuples <> " solutions." $$ ""
  $+$ if nOfTuples == 0 then mempty else vcat $ prettyC tuples
  where
  nOfTuples = T.size tuples
