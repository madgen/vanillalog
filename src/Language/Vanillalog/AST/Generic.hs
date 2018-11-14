{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Vanillalog.AST.Generic where

import Protolude

import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)

import qualified Data.ByteString.Lazy.Char8 as BS

newtype Fact = Fact AtomicFormula deriving (Eq)

data Subgoal op =
    SAtom AtomicFormula
  | SUnOp  (op 'Unary)  (Subgoal op)
  | SBinOp (op 'Binary) (Subgoal op) (Subgoal op)

deriving instance (Eq (op 'Unary), Eq (op 'Binary)) => Eq (Subgoal op)

data SomeOp (op :: OpKind -> *) = NoOp | forall opKind . SomeOp (op opKind)

data OpKind = Binary | Unary

data AtomicFormula = AtomicFormula BS.ByteString [ Term ] deriving (Eq)

data Term = TVar Var | TSym Sym deriving (Eq)

newtype Var = Var BS.ByteString deriving (Eq)
data Sym =
    SymInt  Int
  | SymText BS.ByteString
  | SymBool Bool
  deriving (Eq)

makeBaseFunctor ''Subgoal

operation :: Subgoal op -> SomeOp op
operation SAtom{}         = NoOp
operation (SUnOp op _)    = SomeOp op
operation (SBinOp op _ _) = SomeOp op

vars :: Subgoal a -> [ Var ]
vars = cata alg
  where
  alg :: Base (Subgoal a) [ Var ] -> [ Var ]
  alg (SAtomF (AtomicFormula _ terms)) =
    mapMaybe (\case {TVar v -> Just v; _ -> Nothing}) terms
  alg (SUnOpF _ vars) = vars
  alg (SBinOpF _ vars1 vars2) = vars1 ++ vars2
