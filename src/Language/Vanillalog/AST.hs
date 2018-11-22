{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Vanillalog.AST
  ( Program(..)
  , Sentence(..)
  , Query(..)
  , Clause(..)
  , AG.Fact(..)
  , Subgoal
  , pattern SAtom, pattern SNeg, pattern SConj, pattern SDisj
  , pattern SAtomF, pattern SNegF, pattern SConjF, pattern SDisjF
  , Op(..), AG.OpKind(..), AG.SomeOp(..)
  , AG.AtomicFormula(..)
  , AG.Term(..)
  , AG.Var(..)
  , AG.Sym(..)
  , AG.vars
  , AG.operation
  ) where

import Protolude

import qualified Data.List.NonEmpty as NE


import qualified Language.Exalog.Core as E

import qualified Language.Vanillalog.Generic.AST as AG
import           Language.Vanillalog.Generic.Compiler (ClosureCompilable(..), Closure(..))
import qualified Language.Vanillalog.Generic.Logger as L
import           Language.Vanillalog.Generic.Pretty (Pretty(..), HasPrecedence(..))

type Program = AG.Program Op

type Sentence = AG.Sentence Op

type Query = AG.Query Op

type Clause = AG.Clause Op

type Subgoal = AG.Subgoal Op

data Op (k :: AG.OpKind) where
  Negation    :: Op 'AG.Unary
  Conjunction :: Op 'AG.Binary
  Disjunction :: Op 'AG.Binary

deriving instance Eq (Op opKind)

pattern SAtom span atom      = AG.SAtom span atom
pattern SNeg  span sub       = AG.SUnOp span Negation sub
pattern SConj span sub1 sub2 = AG.SBinOp span Conjunction sub1 sub2
pattern SDisj span sub1 sub2 = AG.SBinOp span Disjunction sub1 sub2

pattern SAtomF span atom          = AG.SAtomF span atom
pattern SNegF  span child         = AG.SUnOpF span Negation child
pattern SConjF span child1 child2 = AG.SBinOpF span Conjunction child1 child2
pattern SDisjF span child1 child2 = AG.SBinOpF span Disjunction child1 child2

-------------------------------------------------------------------------------
-- Pretty printing related instances
-------------------------------------------------------------------------------

instance HasPrecedence Op where
  precedence AG.NoOp                 = 0
  precedence (AG.SomeOp Negation)    = 1
  precedence (AG.SomeOp Conjunction) = 2
  precedence (AG.SomeOp Disjunction) = 3

instance Pretty (Op opKind) where
  pretty Negation    = "!"
  pretty Conjunction = ","
  pretty Disjunction = ";"

-------------------------------------------------------------------------------
-- Compilation related instances
-------------------------------------------------------------------------------

instance ClosureCompilable Op where
  cCompile (CUnary Negation rec)
    | (SAtom{}, core NE.:| []) <- rec =
      pure $ core { E.polarity = E.Negative } NE.:| []
    | otherwise = L.scream L.Impossible Nothing
      "Negation over non-atoms should be eliminated at this point."
  cCompile (CBinary Conjunction (_,core1) (_,core2)) = pure $ append core1 core2
    where
    append :: NE.NonEmpty a -> NE.NonEmpty a -> NE.NonEmpty a
    append (a NE.:| as) (a' NE.:| as') = a NE.:| as ++ a' : as'
  cCompile (CBinary Disjunction _ _) =
    L.scream L.Impossible Nothing
      "Disjunctions should be eliminated at this point."
