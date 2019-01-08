{-# LANGUAGE DataKinds #-}

module Language.Vanillalog.Stage
  ( lex
  , parse
  , namedQueries
  , normalised
  , compiled
  ) where

import Protolude

import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Language.Exalog.Core as E
import qualified Language.Exalog.Relation as R

import           Language.Vanillalog.AST
import qualified Language.Vanillalog.Generic.AST as AG
import           Language.Vanillalog.Generic.Compiler (compile)
import qualified Language.Vanillalog.Generic.Logger as Log
import qualified Language.Vanillalog.Parser.Lexer as Lexer
import qualified Language.Vanillalog.Parser.Parser as Parser
import qualified Language.Vanillalog.Generic.Parser.Lexeme as L
import           Language.Vanillalog.Generic.Transformation.Query (nameQueries)
import           Language.Vanillalog.Transformation.Normaliser (normalise)

type Stage a = FilePath -> BS.ByteString -> Log.LoggerM a

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