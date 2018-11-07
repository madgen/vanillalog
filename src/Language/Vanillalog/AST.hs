module Language.Vanillalog.AST where

import Protolude

import qualified Data.ByteString.Lazy.Char8 as BS

newtype Program = Program [ Either Clause Fact ]

data Clause = Clause AtomicFormula Subgoal

newtype Fact = Fact AtomicFormula

data Subgoal =
    SAtom AtomicFormula
  | SNeg Subgoal
  | SComma Subgoal Subgoal

data AtomicFormula = AtomicFormula BS.ByteString [ Term ]

data Term = TVar Var | TSym Sym

newtype Var = Var BS.ByteString
data Sym =
    SInt  Int
  | SText BS.ByteString
  | SBool Bool
