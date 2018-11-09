{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Language.Vanillalog.AST where

import Protolude

import Data.Functor.Foldable.TH (makeBaseFunctor)

import qualified Data.ByteString.Lazy.Char8 as BS

newtype Program = Program [ Either Clause Fact ]

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
