{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Vanillalog.AST
  ( Program
  , Statement
  , Sentence
  , Query
  , Clause
  , AG.Fact(..)
  , Subgoal
  , pattern SAtom, pattern SNeg, pattern SConj, pattern SDisj
  , pattern SAtomF, pattern SNegF, pattern SConjF, pattern SDisjF
  , Op(..), AG.OpKind(..), AG.SomeOp(..)
  , AG.AtomicFormula(..)
  , AG.Term(..)
  , AG.Var(..)
  , AG.Sym(..)
  , AG.variables
  , AG.operation
  ) where

import Protolude

import qualified Data.List.NonEmpty as NE

import           Language.Exalog.SrcLoc (SrcSpan(NoSpan))
import qualified Language.Exalog.Core as E
import qualified Language.Exalog.Logger as L

import qualified Language.Vanillalog.Generic.AST as AG
import           Language.Vanillalog.Generic.Compiler (ClosureCompilable(..), Closure(..))
import           Language.Vanillalog.Generic.Pretty (Pretty(..), HasPrecedence(..))

type Program = AG.Program Void (Const Void) Op

type Statement = AG.Statement Void (Const Void) Op

type Sentence = AG.Sentence (Const Void) Op

type Query = AG.Query (Const Void) Op

type Clause = AG.Clause (Const Void) Op

type Subgoal = AG.Subgoal

data Op (k :: AG.OpKind) where
  Negation    :: Op 'AG.Unary
  Conjunction :: Op 'AG.Binary
  Disjunction :: Op 'AG.Binary

deriving instance Eq (Op opKind)

pattern SAtom        :: forall (op :: AG.OpKind -> *) term. SrcSpan -> AG.AtomicFormula term                    -> AG.Subgoal op term
pattern SNeg         :: forall                        term. SrcSpan -> AG.Subgoal Op term                       -> AG.Subgoal Op term
pattern SConj, SDisj :: forall                        term. SrcSpan -> AG.Subgoal Op term -> AG.Subgoal Op term -> AG.Subgoal Op term

pattern SAtom span atom      = AG.SAtom span atom
pattern SNeg  span sub       = AG.SUnOp span Negation sub
pattern SConj span sub1 sub2 = AG.SBinOp span Conjunction sub1 sub2
pattern SDisj span sub1 sub2 = AG.SBinOp span Disjunction sub1 sub2

pattern SAtomF         :: forall (op :: AG.OpKind -> *) term r. SrcSpan -> AG.AtomicFormula term -> AG.SubgoalF op term r
pattern SNegF          :: forall                        term r. SrcSpan -> r                     -> AG.SubgoalF Op term r
pattern SConjF, SDisjF :: forall                        term r. SrcSpan -> r -> r                -> AG.SubgoalF Op term r

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

instance HasPrecedence (Const Void) where
  precedence AG.NoOp               = panic "absurd."
  precedence (AG.SomeOp (Const a)) = absurd a

instance Pretty (Op opKind) where
  pretty Negation    = "!"
  pretty Conjunction = ", "
  pretty Disjunction = "; "

instance Pretty Void where
  pretty = absurd

instance Pretty (Const Void (a :: AG.OpKind)) where
  pretty (Const a) = absurd a

-------------------------------------------------------------------------------
-- Compilation related instances
-------------------------------------------------------------------------------

instance ClosureCompilable Op where
  cCompile (CUnary Negation rec)
    | (SAtom{}, core NE.:| []) <- rec =
      pure $ core { E._polarity = E.Negative } NE.:| []
    | otherwise = L.scream NoSpan
      "Negation over non-atoms should be eliminated at this point."
  cCompile (CBinary Conjunction (_,core1) (_,core2)) = pure $ append core1 core2
    where
    append :: NE.NonEmpty a -> NE.NonEmpty a -> NE.NonEmpty a
    append (a NE.:| as) (a' NE.:| as') = a NE.:| as ++ a' : as'
  cCompile (CBinary Disjunction _ _) =
    L.scream NoSpan "Disjunctions should be eliminated at this point."
