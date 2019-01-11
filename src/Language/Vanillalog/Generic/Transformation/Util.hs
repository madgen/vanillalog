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

class Transformable a decl hop bop where
  transformM :: Monad m => (a -> m a) -> Program decl hop bop -> m (Program decl hop bop)
  transform  ::            (a -> a)   -> Program decl hop bop ->    Program decl hop bop
  transform f p = runIdentity $ transformM (pure <$> f) p

instance Transformable (Program decl hop bop) decl hop bop where
  transformM f = f

instance Transformable (Statement decl hop bop) decl hop bop where
  transformM f Program{..} = Program _span <$> traverse f _statements

instance Transformable (Sentence hop bop) decl hop bop where
  transformM :: forall m decl hop bop. Monad m
             => (Sentence hop bop -> m (Sentence hop bop))
             -> Program decl hop bop -> m (Program decl hop bop)
  transformM f = transformM go
    where
    go :: Statement decl hop bop -> m (Statement decl hop bop)
    go StSentence{..} = StSentence _span <$> f _sentence
    go s              = pure s

instance Transformable (Clause hop bop) decl hop bop where
  transformM :: forall m decl hop bop. Monad m
             => (Clause hop bop -> m (Clause hop bop))
             -> Program decl hop bop -> m (Program decl hop bop)
  transformM f = transformM go
    where
    go :: Sentence hop bop -> m (Sentence hop bop)
    go SClause{..} = SClause _span <$> f _clause
    go s           = pure s

instance Transformable (Query hop bop) decl hop bop where
  transformM :: forall m decl hop bop. Monad m
             => (Query hop bop -> m (Query hop bop))
             -> Program decl hop bop -> m (Program decl hop bop)
  transformM f = transformM go
    where
    go :: Sentence hop bop -> m (Sentence hop bop)
    go SQuery{..} = SQuery _span <$> f _query
    go s          = pure s

instance Transformable (Fact hop) decl hop bop where
  transformM :: forall m decl hop bop. Monad m
             => (Fact hop -> m (Fact hop))
             -> Program decl hop bop -> m (Program decl hop bop)
  transformM f = transformM go
    where
    go :: Sentence hop bop -> m (Sentence hop bop)
    go SFact{..} = SFact _span <$> f _fact
    go s         = pure s

instance Transformable (Subgoal Term bop) decl hop bop where
  transformM :: forall m decl hop bop. Monad m
             => (Subgoal Term bop -> m (Subgoal Term bop))
             -> Program decl hop bop -> m (Program decl hop bop)
  transformM f = transformM go
    where
    go :: Sentence hop bop -> m (Sentence hop bop)
    go (SClause s Clause{..}) = SClause s . Clause _span _head <$> f _body
    go (SQuery  s Query{..})  = SQuery  s . Query  _span _head <$> f _body
    go s                      = pure s

-- |Transform only the atomic subgoals in clause/query bodies.
peepholeM :: forall m decl hop bop
           . (Transformable (Subgoal Term bop) decl hop bop, Monad m)
          => (AtomicFormula Term -> m (AtomicFormula Term))
          -> Program decl hop bop -> m (Program decl hop bop)
peepholeM f = transformM go
  where
  go :: Subgoal Term bop -> m (Subgoal Term bop)
  go SAtom{..} = SAtom _span <$> f _atom
  go s         = pure s

peephole :: forall decl hop bop. Transformable (Subgoal Term bop) decl hop bop
         => (AtomicFormula Term -> AtomicFormula Term)
         -> Program decl hop bop -> Program decl hop bop
peephole f = runIdentity . peepholeM (pure <$> f)
