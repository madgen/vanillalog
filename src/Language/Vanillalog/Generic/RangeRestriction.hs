{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Vanillalog.Generic.RangeRestriction where

import Protolude

import Data.List ((\\))

import qualified Language.Exalog.Logger as L

import           Language.Vanillalog.Generic.AST
import           Language.Vanillalog.Generic.Transformation.Util (transformM)

checkRangeRestriction :: forall decl hop bop
                       . Program decl hop bop -> L.Logger ()
checkRangeRestriction = void . transformM (\sent -> handle sent . diff $ sent)
  where
  diff :: Sentence hop bop -> [ Var ]
  diff sent@SFact{_fact = Fact{..}}                   = vars _head
  diff sent@SClause{_clause = Clause{..}}             = vars _head \\ vars _body
  diff sent@SQuery{_query = Query{_head = Just h,..}} = vars  h    \\ vars _body
  diff _                                              = []

  handle sent []                    = pure sent
  handle sent@SFact{} (Var{..} : _) = L.scold (Just _span)
    "Range restriction is violated. Facts can't have variables"
  handle sent@SClause{} (Var{..} : _) = L.scold (Just _span) $
    "Range restriction is violated. " <>
    "All head variables of a clause must appear in its body."
  handle sent@SQuery{} (Var{..} : _) = L.scream (Just _span)
    "Range restriction is violated. Query namer is not working properly."
