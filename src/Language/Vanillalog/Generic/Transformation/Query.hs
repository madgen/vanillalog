{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Vanillalog.Generic.Transformation.Query (nameQueries) where

import Protolude

import           Language.Exalog.Core (PredicateSymbol(..))
import           Language.Exalog.Fresh
import qualified Language.Exalog.Logger as L
import           Language.Exalog.SrcLoc (span)

import           Language.Vanillalog.Generic.AST
import           Language.Vanillalog.Generic.Transformation.Util

nameQueries :: forall decl hop bop
             . [ Text ]
            -> Program decl hop bop
            -> L.Logger (Program decl hop bop)
nameQueries edbReserved pr =
  runFreshT (Just "query_") (edbReserved <> prReserved) (transformM go pr)
  where
  prReserved = (\(PredicateSymbol txt) -> txt) . _predSym <$> atoms @_ @Term pr

  go :: Sentence hop bop -> FreshT L.Logger (Sentence hop bop)
  go (SQuery Query{_head = Nothing, ..}) =
    SQuery <$> (Query _span <$> (Just <$> sub) <*> pure _body)
    where
    sub :: FreshT L.Logger (Subgoal hop Var)
    sub = do
      name <- PredicateSymbol <$> fresh
      pure $ SAtom _span $ AtomicFormula _span name $ variables _body

  go s@SQuery{..} =
    lift $ L.scream (Just . span $ s) "Query has already been named."
  go s = pure s
