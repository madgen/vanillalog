{-# LANGUAGE DataKinds #-}

module Language.Vanillalog.Generic.CLI.Util
  ( succeedOrDie
  , display
  , displayTuples
  ) where

import Protolude hiding (pred)

import System.Exit (exitFailure)

import Text.PrettyPrint hiding ((<>))

import           Data.List (nub, sort, (\\))
import           Data.Text (pack)

import qualified Language.Exalog.Core as E
import           Language.Exalog.Pretty (pp)
import           Language.Exalog.Pretty.Helper (Pretty, prettyC, pretty)
import qualified Language.Exalog.Logger as L
import qualified Language.Exalog.KnowledgeBase.Class as KB
import qualified Language.Exalog.KnowledgeBase.Knowledge as KB
import qualified Language.Exalog.KnowledgeBase.Set as KB
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
        => AG.Program decl hop bop
        -> [ E.PredicateBox 'E.ABase ]
        -> KB.Set 'E.ABase
        -> IO ()
display program queries kb = do
  forM_ kbs $ \kb' ->
    case kb' of
      [] -> panic "Empty knowledge base"
      (KB.Knowledge _ pred _ : _) -> do
        -- Generated query heads contain the span of the overall query
        let querySpan = span pred
        mQuery <- findQueryM program querySpan

        case mQuery of
          Just query -> do
            putStrLn $ pp query
            putStrLn $ pack . render . displayTuples $ kb'
          Nothing -> pure ()

  forM_ emptyPreds $ \(E.PredicateBox pred) -> do
    let querySpan = span pred
    mQuery <- findQueryM program querySpan

    case mQuery of
      Just query -> do
        putStrLn $ pp query
        putStrLn $ pack . render . displayTuples $ []
      Nothing -> pure ()
  where
  kbs :: [ [ KB.Knowledge 'E.ABase ] ]
  kbs = groupBy
          (\(KB.Knowledge _ pred _) (KB.Knowledge _ pred' _)
            -> E.PredicateBox pred == E.PredicateBox pred')
      . nub
      . sort
      $ KB.toList kb

  emptyPreds =
    queries
    \\
    (nub . sort . KB.map (\(KB.Knowledge _ pred _) -> E.PredicateBox pred) $ kb)

findQueryM :: AG.Program decl hop bop -> SrcSpan -> IO (Maybe (AG.Query hop bop))
findQueryM program querySpan = L.runLoggerT (L.LoggerEnv Nothing) $
  case findQuery program querySpan of
    Just query -> pure query
    Nothing    -> L.scream querySpan "This query cannot be found."

findQuery :: AG.Program decl hop bop -> SrcSpan -> Maybe (AG.Query hop bop)
findQuery pr s = find ((== s) . span) (AG.queries pr)

displayTuples :: [ KB.Knowledge a ] -> Doc
displayTuples kb = nest 2 $
     "There are " <>  pretty nOfTuples <> " solutions." $$ ""
  $+$ if nOfTuples == 0 then mempty else vcat (map displayTuple kb)
  where
  nOfTuples = length kb
  displayTuple (KB.Knowledge _ _ syms) = hcat . punctuate "," . prettyC $ syms
