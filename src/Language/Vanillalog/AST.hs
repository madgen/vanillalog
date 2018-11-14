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
  , VanillaSubgoal
  , AG.Subgoal(AG.SAtom)
  , AG.SubgoalF(AG.SAtomF)
  , pattern SNeg, pattern SConj, pattern SDisj
  , pattern SNegF, pattern SConjF, pattern SDisjF
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

newtype Program = Program [ Sentence ]

data Sentence =
    SClause Clause
  | SFact AG.Fact
  | SQuery Query
  deriving (Eq)

data Query = Query
  { head :: Maybe AG.AtomicFormula
  , body :: VanillaSubgoal
  } deriving (Eq)

data Clause = Clause
  { head :: AG.AtomicFormula
  , body :: VanillaSubgoal
  } deriving (Eq)

type VanillaSubgoal = AG.Subgoal Op

data Op (k :: AG.OpKind) where
  Negation    :: Op 'AG.Unary
  Conjunction :: Op 'AG.Binary
  Disjunction :: Op 'AG.Binary

deriving instance Eq (Op opKind)

pattern SNeg sub        = AG.SUnOp Negation sub
pattern SConj sub1 sub2 = AG.SBinOp Conjunction sub1 sub2
pattern SDisj sub1 sub2 = AG.SBinOp Disjunction sub1 sub2

pattern SNegF sub        = AG.SUnOpF Negation sub
pattern SConjF sub1 sub2 = AG.SBinOpF Conjunction sub1 sub2
pattern SDisjF sub1 sub2 = AG.SBinOpF Disjunction sub1 sub2
