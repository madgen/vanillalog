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
--  , peepholeM, peephole
  ) where

import Protolude

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
  transformM f StSentence{..} = StSentence _span <$> f _sentence
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
  transformM f s          = pure s

instance Transformable (Query hop bop) (Statement decl hop bop) where
  transformM f =
    transformM (transformM @(Query hop bop) @(Sentence hop bop) f)

instance Transformable (Query hop bop) (Program decl hop bop) where
  transformM f =
    transformM (transformM @(Query hop bop) @(Statement decl hop bop) f)

instance Transformable (Fact hop) (Sentence hop bop) where
  transformM f SFact{..} = SFact <$> f _fact
  transformM f s         = pure s

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

transformBody = purify transformBodyM

instance Transformable (AtomicFormula a) (Subgoal op a) where
  transformM f SAtom{..} = SAtom _span <$> f _atom
  transformM f s         = pure s

instance Transformable (AtomicFormula Term) (Sentence hop bop) where
  transformM :: forall m decl hop bop. Monad m
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

-- |Transform only the atomic subgoals in clause/query bodies.
peepholeM :: forall m decl hop bop
           . (Transformable (Subgoal bop Term) (Program decl hop bop), Monad m)
          => (AtomicFormula Term -> m (AtomicFormula Term))
          -> Program decl hop bop -> m (Program decl hop bop)
peepholeM f = transformBodyM go
  where
  go :: Subgoal bop Term -> m (Subgoal bop Term)
  go SAtom{..} = SAtom _span <$> f _atom
  go s         = pure s

peephole :: forall decl hop bop. Transformable (Subgoal bop Term) (Program decl hop bop)
         => (AtomicFormula Term -> AtomicFormula Term)
         -> Program decl hop bop -> Program decl hop bop
peephole = purify peepholeM
