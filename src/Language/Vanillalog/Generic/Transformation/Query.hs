{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Vanillalog.Generic.Transformation.Query (nameQueries) where

import Protolude

import Data.String (fromString)

import           Language.Exalog.Core (PredicateSymbol)
import qualified Language.Exalog.Logger as L
import           Language.Exalog.SrcLoc (span)

import           Language.Vanillalog.Generic.AST
import           Language.Vanillalog.Generic.Transformation.Util

nameQueries :: forall decl hop bop
             . Program decl hop bop -> L.Logger (Program decl hop bop)
nameQueries pr = evalStateT (transformM go pr) 0
  where
  go :: Sentence hop bop -> StateT Int L.Logger (Sentence hop bop)
  go (SQuery Query{_head = Nothing, ..}) =
    SQuery <$> (Query _span <$> (Just <$> sub) <*> pure _body)
    where
    sub :: StateT Int L.Logger (Subgoal hop Var)
    sub = do
      name <- freshQueryName
      pure $ SAtom _span $ AtomicFormula _span name $ vars _body

    freshQueryName :: StateT Int L.Logger PredicateSymbol
    freshQueryName = do
      modify (+ 1)
      fromString . ("query_" <>) . show <$> get
  go s@SQuery{..} =
    lift $ L.scream (Just . span $ s) "Query has already been named."
  go s = pure s
