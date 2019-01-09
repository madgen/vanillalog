{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Vanillalog.Generic.RangeRestriction where

import Protolude

import Data.List ((\\))

import           Language.Vanillalog.Generic.AST
import qualified Language.Vanillalog.Generic.Logger as Log
import           Language.Vanillalog.Generic.Transformation.Util (transformM)

checkRangeRestriction :: forall decl op. Program decl op -> Log.LoggerM ()
checkRangeRestriction = void . transformM (\sent -> handle sent . diff $ sent)
  where
  diff :: Sentence op -> [ Var ]
  diff sent@SFact{_fact = Fact{..}}                   = vars _atom
  diff sent@SClause{_clause = Clause{..}}             = vars _head \\ vars _body
  diff sent@SQuery{_query = Query{_head = Just h,..}} = _terms h \\ vars _body
  diff _                                              = []

  handle sent []                    = pure sent
  handle sent@SFact{} (Var{..} : _) = Log.scold (Just _span)
    "Range restriction is violated. Facts can't have variables"
  handle sent@SClause{} (Var{..} : _) = Log.scold (Just _span) $
    "Range restriction is violated. " <>
    "All head variables of a clause must appear in its body."
  handle sent@SQuery{} (Var{..} : _) = Log.scream (Just _span)
    "Range restriction is violated. Query namer is not working properly."
