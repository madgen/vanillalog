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

class Transformable a decl op where
  transformM :: Monad m => (a -> m a) -> Program decl op -> m (Program decl op)
  transform  ::            (a -> a)   -> Program decl op ->    Program decl op
  transform f p = runIdentity $ transformM (pure <$> f) p

instance Transformable (Program decl op) decl op where
  transformM f = f

instance Transformable (Statement decl op) decl op where
  transformM f Program{..} = Program _span <$> traverse f _statements

instance Transformable (Sentence op) decl op where
  transformM :: forall m decl op. Monad m
             => (Sentence op -> m (Sentence op))
             -> Program decl op -> m (Program decl op)
  transformM f = transformM go
    where
    go :: Statement decl op -> m (Statement decl op)
    go StSentence{..} = StSentence _span <$> f _sentence
    go s              = pure s

instance Transformable (Clause op) decl op where
  transformM :: forall m decl op. Monad m
             => (Clause op -> m (Clause op))
             -> Program decl op -> m (Program decl op)
  transformM f = transformM go
    where
    go :: Sentence op -> m (Sentence op)
    go SClause{..} = SClause _span <$> f _clause
    go s           = pure s

instance Transformable (Query op) decl op where
  transformM :: forall m decl op. Monad m
             => (Query op -> m (Query op))
             -> Program decl op -> m (Program decl op)
  transformM f = transformM go
    where
    go :: Sentence op -> m (Sentence op)
    go SQuery{..} = SQuery _span <$> f _query
    go s          = pure s

instance Transformable Fact decl op where
  transformM :: forall m decl op. Monad m
             => (Fact -> m Fact)
             -> Program decl op -> m (Program decl op)
  transformM f = transformM go
    where
    go :: Sentence op -> m (Sentence op)
    go SFact{..} = SFact _span <$> f _fact
    go s         = pure s

instance Transformable (Subgoal op) decl op where
  transformM :: forall m decl op. Monad m
             => (Subgoal op -> m (Subgoal op))
             -> Program decl op -> m (Program decl op)
  transformM f = transformM go
    where
    go :: Sentence op -> m (Sentence op)
    go (SClause s Clause{..}) = SClause s . Clause _span _head <$> f _body
    go (SQuery  s Query{..})  = SQuery  s . Query  _span _head <$> f _body
    go s                      = pure s

-- |Transform only the atomic subgoals in clause/query bodies.
peepholeM :: forall m decl op. (Transformable (Subgoal op) decl op, Monad m)
          => (AtomicFormula -> m AtomicFormula)
          -> Program decl op -> m (Program decl op)
peepholeM f = transformM go
  where
  go :: Subgoal op -> m (Subgoal op)
  go SAtom{..} = SAtom _span <$> f _atom
  go s         = pure s

peephole :: forall decl op. Transformable (Subgoal op) decl op
         => (AtomicFormula -> AtomicFormula)
         -> Program decl op -> Program decl op
peephole f = runIdentity . peepholeM (pure <$> f)
