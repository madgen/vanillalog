{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
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

data Program decl hop bop = Program
  { _span       :: SrcSpan
  , _statements :: [ Statement decl hop bop ]
  }

data Statement decl hop bop =
    StDeclaration { _span :: SrcSpan, _declaration :: decl             }
  | StSentence    { _span :: SrcSpan, _sentence    :: Sentence hop bop }

data Sentence hop bop =
    SClause { _span :: SrcSpan, _clause :: Clause hop bop }
  | SFact   { _span :: SrcSpan, _fact   :: Fact hop       }
  | SQuery  { _span :: SrcSpan, _query  :: Query hop bop  }

data Query hop bop = Query
  { _span :: SrcSpan
  , _head :: Maybe (Subgoal hop Var)
  , _body :: Subgoal bop Term
  }

data Clause hop bop = Clause
  { _span :: SrcSpan
  , _head :: Subgoal hop Term
  , _body :: Subgoal bop Term
  }

data Fact hop = Fact
  { _span :: SrcSpan
  , _head :: Subgoal hop Term
  }

data Subgoal op term =
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
      , _child :: Subgoal op term
      }
  | SBinOp
      { _span   :: SrcSpan
      , _binOp  :: op 'Binary
      , _child1 :: Subgoal op term
      , _child2 :: Subgoal op term
      }
  deriving (Functor)

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
         ) => Eq (Subgoal op term) where
  SAtom{_atom = a}              == SAtom{_atom = a'} = a == a'
  SNullOp{_nullOp = op}         == SNullOp{_nullOp = op'} = op == op'
  SUnOp{_unOp = op, _child = c} == SUnOp{_unOp = op', _child = c'} =
    op == op' && c == c'
  SBinOp{_binOp = op, _child1 = c1, _child2 = c2} ==
    SBinOp{_binOp = op', _child1 = c1', _child2 = c2'} =
    op == op' && c1 == c1' && c2 == c2'

instance (Eq (Subgoal hop Term)) => Eq (Fact hop) where
  Fact{_head = a} == Fact{_head = a'} = a == a'

instance ( Eq (Subgoal hop Term)
         , Eq (Subgoal bop Term)
         ) => Eq (Clause hop bop) where
  Clause{_head = h, _body = b} == Clause{_head = h', _body = b'} =
    h == h' && b == b'

instance ( Eq (Subgoal hop Var)
         , Eq (Subgoal bop Term)
         ) => Eq (Query hop bop) where
  Query{_head = h, _body = b} == Query{_head = h', _body = b'} =
    h == h' && b == b'

instance ( Eq (Fact hop), Eq (Clause hop bop), Eq (Query hop bop)
         ) => Eq (Sentence hop bop) where
  SFact{_fact = f}     == SFact{_fact = f'}     = f == f'
  SQuery{_query = q}   == SQuery{_query = q'}   = q == q'
  SClause{_clause = c} == SClause{_clause = c'} = c == c'

instance (Eq decl , Eq (Sentence hop bop)) => Eq (Statement decl hop bop) where
  StDeclaration{_declaration = d} == StDeclaration{_declaration = d'} = d == d'
  StSentence{_sentence = s}       == StSentence{_sentence = s'}       = s == s'

--------------------------------------------------------------------------------
-- Template Haskell
--------------------------------------------------------------------------------

makeBaseFunctor ''Subgoal

--------------------------------------------------------------------------------
-- Spannable instanes
--------------------------------------------------------------------------------

instance Spannable Term where
  span TVar{..} = span _var
  span TSym{..} = span _sym

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

termType :: Sym -> TermType
termType SymInt{}  = TTInt
termType SymText{} = TTText
termType SymBool{} = TTBool

operation :: Subgoal op term -> SomeOp op
operation SAtom{}     = NoOp
operation s@SNullOp{} = SomeOp (_nullOp s)
operation s@SUnOp{}   = SomeOp (_unOp s)
operation s@SBinOp{}  = SomeOp (_binOp s)

atoms :: Subgoal op term -> [ AtomicFormula term ]
atoms = cata alg
  where
  alg :: Base (Subgoal op term) [ AtomicFormula term ] -> [ AtomicFormula term ]
  alg s@SAtomF{..} = [ _atomF ]
  alg SNullOpF{}   = []
  alg SUnOpF{..}   = _childF
  alg SBinOpF{..}  = _child1F ++ _child2F

class HasVariables a where
  vars :: a -> [ Var ]

instance HasVariables (Sentence hop bop) where
  vars SFact{..}   = vars _fact
  vars SClause{..} = vars _clause
  vars SQuery{..}  = vars _query

instance HasVariables (Clause hop bop) where
  vars Clause{..} = nub $ vars _head ++ vars _body

instance HasVariables (Query hop bop) where
  vars Query{..} = vars _body

instance HasVariables (Fact hop) where
  vars Fact{..} = vars _head

instance HasVariables (AtomicFormula t) => HasVariables (Subgoal op t) where
  vars = nub . cata varAlg
    where
    varAlg (SAtomF _ atom@AtomicFormula{}) = vars atom
    varAlg (SUnOpF _ _ vars)               = vars
    varAlg (SBinOpF _ _ vars1 vars2)       = vars1 ++ vars2

instance HasVariables (AtomicFormula Term) where
  vars AtomicFormula{..} = nub $
    mapMaybe (\case {TVar{..} -> Just _var; _ -> Nothing}) _terms

instance HasVariables (AtomicFormula Var) where
  vars AtomicFormula{..} = nub _terms

instance HasVariables (AtomicFormula Sym) where
  vars AtomicFormula{..} = []

--------------------------------------------------------------------------------
-- IsLabel instances
--------------------------------------------------------------------------------

instance IsLabel "_predSym" (AtomicFormula a -> Text) where
  fromLabel AtomicFormula{..} = _predSym

instance IsLabel "_head" (Fact hop -> Subgoal hop Term) where
  fromLabel Fact{..} = _head

instance IsLabel "_head" (Clause hop bop -> Subgoal hop Term) where
  fromLabel Clause{..} = _head

instance IsLabel "_head" (Query hop bop -> Maybe (Subgoal hop Var)) where
  fromLabel Query{..} = _head
