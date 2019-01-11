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

nameQueries :: forall decl hop bop
             . Transformable (Subgoal Term bop) decl hop bop
            => Program decl hop bop -> L.LoggerM (Program decl hop bop)
nameQueries pr = evalStateT (transformM go pr) 0
  where
  go :: Sentence hop bop -> StateT Int L.LoggerM (Sentence hop bop)
  go (SQuery s Query{_head = Nothing, ..}) =
    SQuery s <$> (Query _span <$> (Just <$> sub) <*> pure _body)
    where
    sub :: StateT Int L.LoggerM (Subgoal Var hop)
    sub = do
      name <- freshQueryName
      pure $ SAtom s $ AtomicFormula s name $ vars _body

    freshQueryName :: StateT Int L.LoggerM Text
    freshQueryName = do
      modify (+ 1)
      pack . ("query_" <>) . show <$> get
  go s@SQuery{..} =
    lift $ L.scream (Just _span) "Query has already been named."
  go s = pure s
