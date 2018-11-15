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

import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Language.Vanillalog.AST.Generic as AG
import           Language.Vanillalog.Pretty (Pretty(..), HasPrecedence(..))

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

pattern SAtom atom      = AG.SAtom atom
pattern SNeg  sub       = AG.SUnOp Negation sub
pattern SConj sub1 sub2 = AG.SBinOp Conjunction sub1 sub2
pattern SDisj sub1 sub2 = AG.SBinOp Disjunction sub1 sub2

pattern SAtomF atom          = AG.SAtomF atom
pattern SNegF  child         = AG.SUnOpF Negation child
pattern SConjF child1 child2 = AG.SBinOpF Conjunction child1 child2
pattern SDisjF child1 child2 = AG.SBinOpF Disjunction child1 child2

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
