{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Vanillalog.Generic.Transformation.Query (nameQueries) where

import Protolude

import qualified Data.ByteString.Lazy.Char8 as BS

import Control.Monad.Trans.Writer (Writer, runWriter, tell)

import Language.Vanillalog.Generic.AST
import Language.Vanillalog.Generic.Transformation.Util

nameQueries :: forall op. Transformable (Subgoal op) op
            => Program op -> Either [ Text ] (Program op)
nameQueries pr =
  case runWriter $ evalStateT (transformM go pr) 0 of
    (pr', errs) | null errs -> Right pr'
                | otherwise -> Left errs
  where
  go :: Sentence op -> StateT Int (Writer [ Text ]) (Sentence op)
  go (SQuery (Query Nothing body)) =
    SQuery <$> (Query <$> (Just <$> ac) <*> pure body)
    where
    ac :: StateT Int (Writer [ Text ]) AtomicFormula
    ac = do
      name <- freshQueryName
      pure $ AtomicFormula name $ TVar <$> vars body

    freshQueryName :: StateT Int (Writer [ Text ]) BS.ByteString
    freshQueryName = do
      modify (+ 1)
      BS.pack . ("query_" <>) . show <$> get
  go s@SQuery{} = do
    lift $ tell [ "Impossible: Query has already been named." ]
    return s
  go s = return s
