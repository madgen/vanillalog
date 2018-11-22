{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Vanillalog.Generic.Transformation.Query (nameQueries) where

import Protolude

import Data.Text (pack)

import           Language.Vanillalog.Generic.AST
import qualified Language.Vanillalog.Generic.Logger as L
import           Language.Vanillalog.Generic.Transformation.Util

nameQueries :: forall op. Transformable (Subgoal op) op
            => Program op -> Either [ Text ] (Program op)
nameQueries pr = L.runLogger $ evalStateT (transformM go pr) 0
  where
  go :: Sentence op -> StateT Int L.LoggerM (Sentence op)
  go (SQuery s Query{_head = Nothing, ..}) =
    SQuery s <$> (Query _span <$> (Just <$> ac) <*> pure _body)
    where
    ac :: StateT Int L.LoggerM AtomicFormula
    ac = do
      name <- freshQueryName
      pure $ AtomicFormula s name $ TVar s <$> vars _body

    freshQueryName :: StateT Int L.LoggerM Text
    freshQueryName = do
      modify (+ 1)
      pack . ("query_" <>) . show <$> get
  go s@SQuery{} =
    lift $ L.scream "Impossible: Query has already been named."
  go s = pure s
