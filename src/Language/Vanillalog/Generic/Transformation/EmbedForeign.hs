{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Vanillalog.Generic.Transformation.EmbedForeign
  ( embedForeign
  ) where

import Protolude

import qualified Data.Map.Strict as M

import qualified Language.Exalog.Core as E
import qualified Language.Exalog.Logger as L

import           Language.Vanillalog.Generic.AST
import           Language.Vanillalog.Generic.Transformation.Util

embedForeign :: forall decl hop bop
              . M.Map E.PredicateSymbol E.SomeForeignFunc
             -> Program decl hop bop
             -> L.Logger (Program decl hop bop)
embedForeign foreignTable = transformM go
  where
  go :: Sentence hop bop -> L.Logger (Sentence hop bop)
  go (SFact Fact{..}) = fmap SFact $ Fact _span
                    <$> transformM (checkForeignClash @Term) _head
  go (SClause Clause{..}) = do
    void $ transformM (checkForeignClash @Term) _head
    pure $ SClause $ Clause _span _head (transform setForeign _body)
  go (SQuery Query{..}) = do
    traverse_ (transformM (checkForeignClash @Var)) _head
    pure $ SQuery $ Query _span _head (transform setForeign _body)

  -- |Check if the predicate name for the atom clashes with a foreign function.
  checkForeignClash :: forall term
                     . AtomicFormula term -> L.Logger (AtomicFormula term)
  checkForeignClash atom@AtomicFormula{..} =
    case _predSym `M.lookup` foreignTable of
      Just _  -> L.scold _span
        "The predicate symbol clashes with that of a foreign predicate."
      Nothing -> pure atom

  -- |Set the foreign function inside the atomic formula
  setForeign :: AtomicFormula Term -> AtomicFormula Term
  setForeign atom@AtomicFormula{..}=
    case _predSym `M.lookup` foreignTable of
      Just someForeign -> AtomicFormula{_nature = Just someForeign, ..}
      Nothing          -> atom
