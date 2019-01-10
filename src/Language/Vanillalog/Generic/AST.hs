{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Vanillalog.Generic.AST where

import Protolude

import Data.List (nub)

import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)

import GHC.OverloadedLabels

import Language.Vanillalog.Generic.Parser.SrcLoc

data Program decl op = Program
  { _span       :: SrcSpan
  , _statements :: [ Statement decl op ]
  }

data Statement decl op =
    StDeclaration { _span :: SrcSpan, _declaration :: decl }
  | StSentence    { _span :: SrcSpan, _sentence    :: Sentence op }

data Sentence op =
    SClause { _span :: SrcSpan, _clause :: Clause op }
  | SFact   { _span :: SrcSpan, _fact   :: Fact      }
  | SQuery  { _span :: SrcSpan, _query  :: Query op  }

data Query op = Query
  { _span :: SrcSpan
  , _head :: Maybe (AtomicFormula Var)
  , _body :: Subgoal Term op
  }

data Clause op = Clause
  { _span :: SrcSpan
  , _head :: AtomicFormula Term
  , _body :: Subgoal Term op
  }

data Fact = Fact
  { _span :: SrcSpan
  , _head :: AtomicFormula Term
  }

data Subgoal term op =
    SAtom
      { _span :: SrcSpan
      , _atom :: AtomicFormula term
      }
  | SNullOp
      { _span   :: SrcSpan
      , _nullOp :: op 'Nullary
      }
  | SUnOp
      { _span  :: SrcSpan
      , _unOp  :: op 'Unary
      , _child :: Subgoal term op
      }
  | SBinOp
      { _span   :: SrcSpan
      , _binOp  :: op 'Binary
      , _child1 :: Subgoal term op
      , _child2 :: Subgoal term op
      }

data SomeOp (op :: OpKind -> *) = NoOp | forall opKind . SomeOp (op opKind)

data OpKind = Binary | Unary | Nullary

data AtomicFormula a =
  AtomicFormula
    { _span    :: SrcSpan
    , _predSym :: Text
    , _terms   :: [ a ]
    } deriving (Functor, Foldable, Traversable)

data Term =
    TVar { _var :: Var }
  | TSym { _sym :: Sym }
  deriving (Eq)

data TermType = TTInt | TTText | TTBool deriving (Eq)

data Var = Var { _span :: SrcSpan, _varName :: Text }
data Sym =
    SymInt  { _span :: SrcSpan, _int  :: Int  }
  | SymText { _span :: SrcSpan, _text :: Text }
  | SymBool { _span :: SrcSpan, _bool :: Bool }

--------------------------------------------------------------------------------
-- Eq instances
--------------------------------------------------------------------------------

instance Eq Sym where
  SymInt{_int = i}   == SymInt{_int = i'}   = i == i'
  SymBool{_bool = b} == SymBool{_bool = b'} = b == b'
  SymText{_text = t} == SymText{_text = t'} = t == t'
  _ == _ = False

instance Eq Var where
  Var{_varName = v} == Var{_varName = v'} = v == v'

instance Eq a => Eq (AtomicFormula a) where
  AtomicFormula{_predSym = sym, _terms = ts} ==
    AtomicFormula{_predSym = sym', _terms = ts'} = sym == sym' && ts == ts'

instance ( Eq (op 'Nullary), Eq (op 'Unary), Eq (op 'Binary)
         , Eq (AtomicFormula term)
         ) => Eq (Subgoal term op) where
  SAtom{_atom = a}              == SAtom{_atom = a'} = a == a'
  SNullOp{_nullOp = op}         == SNullOp{_nullOp = op'} = op == op'
  SUnOp{_unOp = op, _child = c} == SUnOp{_unOp = op', _child = c'} =
    op == op' && c == c'
  SBinOp{_binOp = op, _child1 = c1, _child2 = c2} ==
    SBinOp{_binOp = op', _child1 = c1', _child2 = c2'} =
    op == op' && c1 == c1' && c2 == c2'

instance Eq Fact where
  Fact{_head = a} == Fact{_head = a'} = a == a'

instance (Eq (op 'Nullary), Eq (op 'Unary), Eq (op 'Binary))
    => Eq (Clause op) where
  Clause{_head = h, _body = b} == Clause{_head = h', _body = b'} =
    h == h' && b == b'

instance (Eq (op 'Nullary), Eq (op 'Unary), Eq (op 'Binary))
    => Eq (Query op) where
  Query{_head = h, _body = b} == Query{_head = h', _body = b'} =
    h == h' && b == b'

instance (Eq (op 'Nullary), Eq (op 'Unary), Eq (op 'Binary))
    => Eq (Sentence op) where
  SFact{_fact = f}     == SFact{_fact = f'}     = f == f'
  SQuery{_query = q}   == SQuery{_query = q'}   = q == q'
  SClause{_clause = c} == SClause{_clause = c'} = c == c'

instance (Eq decl, Eq (op 'Nullary), Eq (op 'Unary), Eq (op 'Binary))
    => Eq (Statement decl op) where
  StDeclaration{_declaration = d} == StDeclaration{_declaration = d'} = d == d'
  StSentence{_sentence = s}       == StSentence{_sentence = s'}       = s == s'

--------------------------------------------------------------------------------
-- Template Haskell
--------------------------------------------------------------------------------

makeBaseFunctor ''Subgoal

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

termType :: Sym -> TermType
termType SymInt{}  = TTInt
termType SymText{} = TTText
termType SymBool{} = TTBool

operation :: Subgoal term op -> SomeOp op
operation SAtom{}     = NoOp
operation s@SNullOp{} = SomeOp (_nullOp s)
operation s@SUnOp{}   = SomeOp (_unOp s)
operation s@SBinOp{}  = SomeOp (_binOp s)

atoms :: Subgoal term op -> [ AtomicFormula term ]
atoms = cata alg
  where
  alg :: Base (Subgoal term op) [ AtomicFormula term ] -> [ AtomicFormula term ]
  alg s@SAtomF{..} = [ _atomF ]
  alg SNullOpF{}   = []
  alg SUnOpF{..}   = _childF
  alg SBinOpF{..}  = _child1F ++ _child2F

class HasVariables a where
  vars :: a -> [ Var ]

instance HasVariables (Sentence op) where
  vars SFact{..}   = vars _fact
  vars SClause{..} = vars _clause
  vars SQuery{..}  = vars _query

instance HasVariables (Clause op) where
  vars Clause{..} = nub $ vars _head ++ vars _body

instance HasVariables (Query op) where
  vars Query{..} = vars _body

instance HasVariables Fact where
  vars Fact{..} = vars _head

instance HasVariables (Subgoal Term op) where
  vars = nub . cata alg
    where
    alg :: Base (Subgoal Term a) [ Var ] -> [ Var ]
    alg (SAtomF _ atom@AtomicFormula{}) = vars atom
    alg (SUnOpF _ _ vars)               = vars
    alg (SBinOpF _ _ vars1 vars2)       = vars1 ++ vars2

instance HasVariables (AtomicFormula Term) where
  vars AtomicFormula{..} = nub $
    mapMaybe (\case {TVar{..} -> Just _var; _ -> Nothing}) _terms

--------------------------------------------------------------------------------
-- IsLabel instances
--------------------------------------------------------------------------------

instance IsLabel "_predSym" (AtomicFormula a -> Text) where
  fromLabel AtomicFormula{..} = _predSym

instance IsLabel "_head" (Fact -> AtomicFormula Term) where
  fromLabel Fact{..} = _head

instance IsLabel "_head" (Clause op -> AtomicFormula Term) where
  fromLabel Clause{..} = _head

instance IsLabel "_head" (Query op -> Maybe (AtomicFormula Var)) where
  fromLabel Query{..} = _head
