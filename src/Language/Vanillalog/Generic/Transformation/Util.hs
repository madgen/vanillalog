{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Vanillalog.Generic.Transformation.Util where

import Protolude

import Language.Vanillalog.Generic.AST

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

class Transformable a op where
  transformM :: Monad m => (a -> m a) -> Program op -> m (Program op)
  transform  ::            (a -> a)   -> Program op -> Program op
  transform f p = runIdentity $ transformM (pure <$> f) p

instance Transformable (Program op) op where
  transformM f = f

instance Transformable (Sentence op) op where
  transformM f Program{..} = Program _span <$> traverse f _sentences

instance Transformable (Clause op) op where
  transformM :: forall m op. Monad m
             => (Clause op -> m (Clause op)) -> Program op -> m (Program op)
  transformM f = transformM go
    where
    go :: Sentence op -> m (Sentence op)
    go SClause{..} = SClause _span <$> f _clause
    go s           = pure s

instance Transformable (Query op) op where
  transformM :: forall m op. Monad m
             => (Query op -> m (Query op)) -> Program op -> m (Program op)
  transformM f = transformM go
    where
    go :: Sentence op -> m (Sentence op)
    go SQuery{..} = SQuery _span <$> f _query
    go s          = pure s

instance Transformable Fact op where
  transformM :: forall m op. Monad m
             => (Fact -> m Fact) -> Program op -> m (Program op)
  transformM f = transformM go
    where
    go :: Sentence op -> m (Sentence op)
    go SFact{..} = SFact _span <$> f _fact
    go s         = pure s

instance Transformable (Subgoal op) op where
  transformM :: forall m op. Monad m
             => (Subgoal op -> m (Subgoal op)) -> Program op -> m (Program op)
  transformM f = transformM go
    where
    go :: Sentence op -> m (Sentence op)
    go (SClause s Clause{..}) = SClause s . Clause _span _head <$> f _body
    go (SQuery  s Query{..})  = SQuery  s . Query  _span _head <$> f _body
    go s                      = pure s

-- |Transform only the atomic subgoals in clause/query bodies.
peepholeM :: forall op m. (Transformable (Subgoal op) op, Monad m)
          => (AtomicFormula -> m AtomicFormula) -> Program op -> m (Program op)
peepholeM f = transformM go
  where
  go :: Subgoal op -> m (Subgoal op)
  go SAtom{..} = SAtom _span <$> f _atom
  go s         = pure s

peephole :: forall op. Transformable (Subgoal op) op
         => (AtomicFormula -> AtomicFormula) -> Program op -> Program op
peephole f = runIdentity . peepholeM (pure <$> f)
