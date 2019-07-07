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
  ) where

import Protolude

import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Language.Exalog.Core as E
import qualified Language.Exalog.Relation as R
import           Language.Exalog.Renamer (rename)
import qualified Language.Exalog.Logger as Log
import           Language.Exalog.RangeRestriction (fixRangeRestriction)
import           Language.Exalog.WellModing (fixModing)
import           Language.Exalog.Stratification (stratify)

import           Language.Vanillalog.AST
import           Language.Vanillalog.Generic.Compiler (compile)
import qualified Language.Vanillalog.Parser.Lexer as Lexer
import qualified Language.Vanillalog.Parser.Parser as Parser
import qualified Language.Vanillalog.Generic.Parser.Lexeme as L
import           Language.Vanillalog.Generic.Transformation.Query (nameQueries)
import           Language.Vanillalog.Transformation.Normaliser (normalise)

type Stage a = FilePath -> BS.ByteString -> Log.Logger a

lex :: Stage [ L.Lexeme (Lexer.Token Text) ]
lex = Lexer.lex

parse :: Stage Program
parse = Parser.programParser

namedQueries :: Stage Program
namedQueries file = parse file >=> nameQueries

normalised :: Stage Program
normalised file = namedQueries file >=> normalise

compiled :: Stage (E.Program 'E.ABase, R.Solution 'E.ABase)
compiled file = normalised file >=> compile

rangeRestrictionRepaired :: Stage (E.Program 'E.ABase, R.Solution 'E.ABase)
rangeRestrictionRepaired file = compiled file
                            >=> rename
                            >=> fixRangeRestriction

wellModed :: Stage (E.Program 'E.ABase, R.Solution 'E.ABase)
wellModed file = rangeRestrictionRepaired file
             >=> rename
             >=> fixModing

stratified :: Stage (E.Program 'E.ABase, R.Solution 'E.ABase)
stratified file bs = do
  (pr, sol) <- wellModed file bs
  pr' <- stratify $ E.decorate pr
  pure (pr', sol)
