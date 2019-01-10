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

nameQueries :: forall decl op. Transformable (Subgoal Term op) decl op
            => Program decl op -> L.LoggerM (Program decl op)
nameQueries pr = evalStateT (transformM go pr) 0
  where
  go :: Sentence op -> StateT Int L.LoggerM (Sentence op)
  go (SQuery s Query{_head = Nothing, ..}) =
    SQuery s <$> (Query _span <$> (Just <$> ac) <*> pure _body)
    where
    ac :: StateT Int L.LoggerM (AtomicFormula Var)
    ac = do
      name <- freshQueryName
      pure $ AtomicFormula s name $ vars _body

    freshQueryName :: StateT Int L.LoggerM Text
    freshQueryName = do
      modify (+ 1)
      pack . ("query_" <>) . show <$> get
  go s@SQuery{..} =
    lift $ L.scream (Just _span) "Query has already been named."
  go s = pure s
