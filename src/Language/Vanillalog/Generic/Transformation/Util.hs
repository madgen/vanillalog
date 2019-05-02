{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Vanillalog.Generic.Transformation.Util
  ( Transformable(..)
  , Algebra, Coalgebra
  , transformHeadM, transformHead
  , transformBodyM, transformBody
  ) where

import Protolude

import Data.Functor.Foldable (cata, embed, Base)

import Language.Vanillalog.Generic.AST

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

purify :: ((a -> Identity a) -> (b -> Identity b)) -> (a -> a) -> (b -> b)
purify f g = runIdentity . f (pure <$> g)

class Transformable a b where
  transformM :: Monad m => (a -> m a) -> b -> m b
  transform  ::            (a ->   a) -> b ->   b
  transform = purify transformM

instance Transformable (Statement decl hop bop) (Program decl hop bop) where
  transformM f Program{..} = Program _span <$> traverse f _statements

instance Transformable (Sentence hop bop) (Statement decl hop bop) where
  transformM f StSentence{..} = StSentence <$> f _sentence
  transformM _ s              = pure s

instance Transformable (Sentence hop bop) (Program decl hop bop) where
  transformM f =
    transformM (transformM @(Sentence hop bop) @(Statement decl hop bop) f)

instance Transformable (Clause hop bop) (Sentence hop bop) where
  transformM f SClause{..} = SClause <$> f _clause
  transformM _ s           = pure s

instance Transformable (Clause hop bop) (Statement decl hop bop) where
  transformM f =
    transformM (transformM @(Clause hop bop) @(Sentence hop bop) f)

instance Transformable (Clause hop bop) (Program decl hop bop) where
  transformM f =
    transformM (transformM @(Clause hop bop) @(Statement decl hop bop) f)

instance Transformable (Query hop bop) (Sentence hop bop) where
  transformM f SQuery{..} = SQuery <$> f _query
  transformM _ s          = pure s

instance Transformable (Query hop bop) (Statement decl hop bop) where
  transformM f =
    transformM (transformM @(Query hop bop) @(Sentence hop bop) f)

instance Transformable (Query hop bop) (Program decl hop bop) where
  transformM f =
    transformM (transformM @(Query hop bop) @(Statement decl hop bop) f)

instance Transformable (Fact hop) (Sentence hop bop) where
  transformM f SFact{..} = SFact <$> f _fact
  transformM _ s         = pure s

instance Transformable (Fact hop) (Statement decl hop bop) where
  transformM f =
    transformM (transformM @(Fact hop) @(Sentence hop bop) f)

instance Transformable (Fact hop) (Program decl hop bop) where
  transformM f =
    transformM (transformM @(Fact hop) @(Statement decl hop bop) f)

transformHeadM :: forall m decl hop bop. Monad m
               => (Subgoal hop Term -> m (Subgoal hop Term))
               -> Program decl hop bop -> m (Program decl hop bop)
transformHeadM f = transformM go
  where
  go :: Sentence hop bop -> m (Sentence hop bop)
  go (SClause Clause{..}) = (\h -> SClause Clause{_head = h,..}) <$> f _head
  go (SFact   Fact{..})   = (\h -> SFact   Fact{_head = h,..})   <$> f _head
  go s@SQuery{}           = pure s

transformHead :: forall decl hop bop
               . (Subgoal hop Term -> Subgoal hop Term)
              -> Program decl hop bop -> Program decl hop bop
transformHead = purify transformHeadM

transformBodyM :: forall m decl hop bop. Monad m
               => (Subgoal bop Term -> m (Subgoal bop Term))
               -> Program decl hop bop -> m (Program decl hop bop)
transformBodyM f = transformM go
  where
  go :: Sentence hop bop -> m (Sentence hop bop)
  go (SClause Clause{..}) = SClause . Clause _span _head <$> f _body
  go (SQuery  Query{..})  = SQuery  . Query  _span _head <$> f _body
  go s                    = pure s

transformBody :: forall decl hop bop
               . (Subgoal bop Term -> Subgoal bop Term)
              -> Program decl hop bop -> Program decl hop bop
transformBody = purify transformBodyM

instance Transformable (AtomicFormula a) (Subgoal op a) where
  transformM :: forall m. Monad m
             => (AtomicFormula a -> m (AtomicFormula a))
             -> Subgoal op a -> m (Subgoal op a)
  transformM f = cata alg
    where
    alg :: Algebra (Base (Subgoal op a)) (m (Subgoal op a))
    alg (SAtomF span atom) = SAtom span <$> f atom
    alg subf               = embed <$> sequence subf

instance Transformable (AtomicFormula Term) (Sentence hop bop) where
  transformM :: forall m. Monad m
             => (AtomicFormula Term -> m (AtomicFormula Term))
             -> Sentence hop bop -> m (Sentence hop bop)
  transformM f = go
    where
    f' :: forall op. Subgoal op Term -> m (Subgoal op Term)
    f' = transformM f

    go :: Sentence hop bop -> m (Sentence hop bop)
    go (SClause Clause{..}) = (\h -> SClause . Clause _span h)    <$> f' _head <*> f' _body
    go (SQuery  Query{..})  =        SQuery  . Query  _span _head <$> f' _body
    go (SFact   Fact{..})   =        SFact   . Fact   _span       <$> f' _head

instance Transformable (AtomicFormula Term) (Statement decl hop bop) where
  transformM f =
    transformM (transformM @(AtomicFormula Term) @(Sentence hop bop) f)

instance Transformable (AtomicFormula Term) (Program decl hop bop) where
  transformM f =
    transformM (transformM @(AtomicFormula Term) @(Statement decl hop bop) f)
