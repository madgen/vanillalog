{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Vanillalog.Generic.Transformation.Util where

import Protolude

import Language.Vanillalog.Generic.AST

class Transformable a op where
  transform :: (a -> a) -> Program op -> Program op

instance Transformable (Program op) op where
  transform f p = f p

instance Transformable (Sentence op) op where
  transform f (Program sentences) = Program (f <$> sentences)

instance Transformable (Clause op) op where
  transform f = transform go
    where
    go :: Sentence op -> Sentence op
    go (SClause cl) = SClause (f cl)
    go s = s

instance Transformable (Query op) op where
  transform f = transform go
    where
    go :: Sentence op -> Sentence op
    go (SQuery q) = SQuery (f q)
    go s = s

instance Transformable Fact op where
  transform f = transform go
    where
    go :: Sentence op -> Sentence op
    go (SFact fact) = SFact (f fact)
    go s = s

instance Transformable (Subgoal op) op where
  transform f = transform go
    where
    go :: Sentence op -> Sentence op
    go (SClause (Clause head body)) = SClause (Clause head (f body))
    go (SQuery  (Query  head body)) = SQuery  (Query  head (f body))
    go s                            = s

-- |Transform only the atomic subgoals in clause/query bodies.
peephole :: forall op. Transformable (Subgoal op) op
         => (AtomicFormula -> AtomicFormula) -> Program op -> Program op
peephole f = transform go
  where
  go :: Subgoal op -> Subgoal op
  go (SAtom atom) = SAtom (f atom)
  go s = s
