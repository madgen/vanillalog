{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Vanillalog.Stage
  ( lex
  , parse
  , foreignEmbedded
  , namedQueries
  , normalised
  , compiled
  , rangeRestrictionRepaired
  , wellModed
  , stratified
  , solved
  , module Stage
  ) where

import Protolude

import qualified Language.Exalog.Core as E
import           Language.Exalog.Renamer (rename)
import qualified Language.Exalog.KnowledgeBase.Set as KB
import qualified Language.Exalog.Solver as Solver
import           Language.Exalog.SrcLoc (span, SrcSpan(NoSpan))
import           Language.Exalog.RangeRestriction (fixRangeRestriction)
import           Language.Exalog.WellModing (fixModing)
import           Language.Exalog.Stratification (stratify)
import qualified Language.Exalog.Logger as Log

import           Language.Vanillalog.AST
import qualified Language.Vanillalog.Foreign as FFI
import           Language.Vanillalog.Generic.Stage as Stage
import           Language.Vanillalog.Generic.Compiler (compile)
import qualified Language.Vanillalog.Parser.Lexer as Lexer
import qualified Language.Vanillalog.Parser.Parser as Parser
import qualified Language.Vanillalog.Generic.AST as G
import qualified Language.Vanillalog.Generic.Parser.Lexeme as L
import           Language.Vanillalog.Generic.Transformation.Query (nameQueries)
import qualified Language.Vanillalog.Generic.Transformation.EmbedForeign as FFI
import           Language.Vanillalog.Transformation.Normaliser (normalise)

lex :: Stage Void (Const Void) Op [ L.Lexeme (Lexer.Token Text) ]
lex = do
  inp <- _input <$> ask

  (inpSrc, src) <- case inp of
    Textual{_inputSource, _source} -> pure (_inputSource, _source)
    _ -> lift $ Log.scream NoSpan "Can't lex without textual input."

  lift $ Lexer.lex inpSrc src

parse :: Stage Void (Const Void) Op Program
parse = do
  inp <- _input <$> ask

  (inpSrc, src) <- case inp of
    Textual{_inputSource, _source} -> pure (_inputSource, _source)
    _ -> lift $ Log.scream NoSpan "Can't parse without textual input."

  case _parserScope inp of
    SProgram  -> lift $ Parser.programParser inpSrc src
    SSentence -> do
      query <- lift $ Parser.replParser inpSrc src
      pure $ G.Program (span query) [ G.StSentence (G.SQuery query) ]

foreignEmbedded :: Stage Void (Const Void) Op Program
foreignEmbedded = do
  inp <- _input <$> ask
  ast <- case inp of
    Textual{} -> parse
    AST ast   -> pure ast

  lift $ FFI.embedForeign FFI.foreignTable ast

namedQueries :: Stage Void (Const Void) Op Program
namedQueries = do
  ast <- foreignEmbedded
  reserved <- _reservedNames <$> ask
  lift $ nameQueries reserved ast

normalised :: Stage Void (Const Void) Op Program
normalised = namedQueries >>= lift . normalise

compiled :: Stage Void (Const Void) Op (E.Program 'E.ABase, KB.Set 'E.ABase)
compiled = normalised >>= lift . compile

rangeRestrictionRepaired :: Stage Void (Const Void) Op (E.Program 'E.ABase, KB.Set 'E.ABase)
rangeRestrictionRepaired = compiled
                       >>= lift . rename
                       >>= lift . fixRangeRestriction

wellModed :: Stage Void (Const Void) Op (E.Program 'E.ABase, KB.Set 'E.ABase)
wellModed = rangeRestrictionRepaired
        >>= lift . rename
        >>= lift . fixModing

stratified :: Stage Void (Const Void) Op (E.Program 'E.ABase, KB.Set 'E.ABase)
stratified = do
  (pr, sol) <- wellModed
  pr' <- lift $ stratify $ E.decorate pr
  pure (pr', sol)

solved :: KB.Set 'E.ABase -> Stage Void (Const Void) Op (KB.Set 'E.ABase)
solved baseEDB = do
  (program, initEDB) <- stratified
  keepPredicates <- _keepPredicates <$> ask
  let edb = baseEDB <> initEDB
  lift $ case keepPredicates of
    OnlyQueryPreds -> Solver.solve program edb
    AllPreds       -> Solver.solve (mkEveryPredQueriable program) edb
  where
  mkEveryPredQueriable :: E.Program 'E.ABase -> E.Program 'E.ABase
  mkEveryPredQueriable pr@E.Program{..} =
    E.Program{_queries = E.predicates pr,..}
