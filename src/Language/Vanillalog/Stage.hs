{-# LANGUAGE DataKinds #-}

module Language.Vanillalog.Stage
  ( lex
  , parse
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
import qualified Language.Exalog.Relation as R
import           Language.Exalog.Renamer (rename)
import qualified Language.Exalog.Solver as Solver
import           Language.Exalog.SrcLoc (span)
import           Language.Exalog.RangeRestriction (fixRangeRestriction)
import           Language.Exalog.WellModing (fixModing)
import           Language.Exalog.Stratification (stratify)

import           Language.Vanillalog.AST
import           Language.Vanillalog.Generic.Stage as Stage
import           Language.Vanillalog.Generic.Compiler (compile)
import qualified Language.Vanillalog.Parser.Lexer as Lexer
import qualified Language.Vanillalog.Parser.Parser as Parser
import qualified Language.Vanillalog.Generic.AST as G
import qualified Language.Vanillalog.Generic.Parser.Lexeme as L
import           Language.Vanillalog.Generic.Transformation.Query (nameQueries)
import           Language.Vanillalog.Transformation.Normaliser (normalise)

lex :: Stage [ L.Lexeme (Lexer.Token Text) ]
lex = do
  env <- ask
  lift $ Lexer.lex (_file env) (_input env)

parse :: Stage Program
parse = do
  env <- ask
  case _parserScope env of
    SProgram  -> lift $ Parser.programParser (_file env) (_input env)
    SSentence -> do
      sent <- lift $ Parser.sentenceParser (_file env) (_input env)
      pure $ G.Program (span sent) [ G.StSentence sent ]

namedQueries :: Stage Program
namedQueries = parse >>= lift . nameQueries

normalised :: Stage Program
normalised = namedQueries >>= lift . normalise

compiled :: Stage (E.Program 'E.ABase, R.Solution 'E.ABase)
compiled = normalised >>= lift . compile

rangeRestrictionRepaired :: Stage (E.Program 'E.ABase, R.Solution 'E.ABase)
rangeRestrictionRepaired = compiled
                       >>= lift . rename
                       >>= lift . fixRangeRestriction

wellModed :: Stage (E.Program 'E.ABase, R.Solution 'E.ABase)
wellModed = rangeRestrictionRepaired
        >>= lift . rename
        >>= lift . fixModing

stratified :: Stage (E.Program 'E.ABase, R.Solution 'E.ABase)
stratified = do
  (pr, sol) <- wellModed
  pr' <- lift $ stratify $ E.decorate pr
  pure (pr', sol)

solved :: Stage (R.Solution 'E.ABase)
solved = stratified >>= lift . uncurry Solver.solve
