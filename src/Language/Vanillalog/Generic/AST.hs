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

module Language.Vanillalog.Generic.AST where

import Protolude

import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)

import Language.Vanillalog.Generic.Parser.SrcLoc

data Program op = Program
  { _span      :: SrcSpan
  , _sentences :: [ Sentence op ]
  }

data Sentence op =
    SClause { _span :: SrcSpan, _clause :: Clause op }
  | SFact   { _span :: SrcSpan, _fact   :: Fact      }
  | SQuery  { _span :: SrcSpan, _query  :: Query op  }

deriving instance (Eq (op 'Unary), Eq (op 'Binary)) => Eq (Sentence op)

data Query op = Query
  { _span :: SrcSpan
  , _head :: Maybe AtomicFormula
  , _body :: Subgoal op
  }

deriving instance (Eq (op 'Unary), Eq (op 'Binary)) => Eq (Query op)

data Clause op = Clause
  { _span :: SrcSpan
  , _head :: AtomicFormula
  , _body :: Subgoal op
  }

deriving instance (Eq (op 'Unary), Eq (op 'Binary)) => Eq (Clause op)

data Fact = Fact
  { _span :: SrcSpan
  , _atom :: AtomicFormula
  } deriving (Eq)

data Subgoal op =
    SAtom
      { _span :: SrcSpan
      , _atom :: AtomicFormula
      }
  | SUnOp
      { _span  :: SrcSpan
      , _unOp  :: op 'Unary
      , _child :: Subgoal op
      }
  | SBinOp
      { _span   :: SrcSpan
      , _binOp  :: op 'Binary
      , _child1 :: Subgoal op
      , _child2 :: Subgoal op
      }

deriving instance (Eq (op 'Unary), Eq (op 'Binary)) => Eq (Subgoal op)

data SomeOp (op :: OpKind -> *) = NoOp | forall opKind . SomeOp (op opKind)

data OpKind = Binary | Unary

data AtomicFormula =
  AtomicFormula
    { _span  :: SrcSpan
    , _fxSym :: Text
    , _terms :: [ Term ]
    } deriving (Eq)

data Term =
    TVar
      { _span :: SrcSpan
      , _var  :: Var
      }
  | TSym
      { _span :: SrcSpan
      , _sym  :: Sym
      } deriving (Eq)

newtype Var = Var Text deriving (Eq)
data Sym =
    SymInt  Int
  | SymText Text
  | SymBool Bool
  deriving (Eq)

makeBaseFunctor ''Subgoal

operation :: Subgoal op -> SomeOp op
operation SAtom{}    = NoOp
operation s@SUnOp{}  = SomeOp (_unOp s)
operation s@SBinOp{} = SomeOp (_binOp s)

vars :: Subgoal a -> [ Var ]
vars = cata alg
  where
  alg :: Base (Subgoal a) [ Var ] -> [ Var ]
  alg (SAtomF _ (AtomicFormula _ _ terms)) =
    mapMaybe (\case {TVar _ v -> Just v; _ -> Nothing}) terms
  alg (SUnOpF _ _ vars) = vars
  alg (SBinOpF _ _ vars1 vars2) = vars1 ++ vars2
