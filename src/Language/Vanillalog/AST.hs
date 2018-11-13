{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Vanillalog.AST where

import Protolude

import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)

import qualified Data.ByteString.Lazy.Char8 as BS

newtype Program = Program [ Sentence ]

data Sentence =
    SClause Clause
  | SFact Fact
  | SQuery Query
  deriving (Eq)

data Query = Query
  { head :: Maybe AtomicFormula
  , body :: Subgoal
  } deriving (Eq)

data Clause = Clause
  { head :: AtomicFormula
  , body :: Subgoal
  } deriving (Eq)

newtype Fact = Fact AtomicFormula deriving (Eq)

data Subgoal =
    SAtom AtomicFormula
  | SNeg Subgoal
  | SConj Subgoal Subgoal
  | SDisj Subgoal Subgoal
  deriving (Eq)

data AtomicFormula = AtomicFormula BS.ByteString [ Term ] deriving (Eq)

data Term = TVar Var | TSym Sym deriving (Eq)

newtype Var = Var BS.ByteString deriving (Eq)
data Sym =
    SymInt  Int
  | SymText BS.ByteString
  | SymBool Bool
  deriving (Eq)

makeBaseFunctor ''Subgoal

vars :: Subgoal -> [ Var ]
vars = cata alg
  where
  alg :: Base Subgoal [ Var ] -> [ Var ]
  alg (SAtomF (AtomicFormula _ terms)) =
    mapMaybe (\case {TVar v -> Just v; _ -> Nothing}) terms
  alg (SNegF vars) = vars
  alg (SConjF vars1 vars2) = vars1 ++ vars2
  alg (SDisjF vars1 vars2) = vars1 ++ vars2
