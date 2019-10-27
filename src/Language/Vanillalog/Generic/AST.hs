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

import Protolude hiding (sym)

import Data.List (nub)

import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)

import Language.Exalog.Core (PredicateSymbol, SomeNature)
import Language.Exalog.SrcLoc

data Program decl hop bop = Program
  { _span       :: SrcSpan
  , _statements :: [ Statement decl hop bop ]
  }

data Statement decl hop bop =
    StDeclaration { _declaration :: decl             }
  | StSentence    { _sentence    :: Sentence hop bop }

data Sentence hop bop =
    SClause { _clause :: Clause hop bop }
  | SFact   { _fact   :: Fact hop       }
  | SQuery  { _query  :: Query hop bop  }

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
    , _predSym :: PredicateSymbol
    , _nature  :: SomeNature
    , _terms   :: [ a ]
    } deriving (Functor, Foldable, Traversable)

data Term =
    TVar  { _var  :: Var }
  | TSym  { _sym  :: Sym }
  | TWild { _span :: SrcSpan }
  deriving (Eq,Ord)

data TermType = TTInt | TTText | TTBool deriving (Eq, Ord)

data Var = Var { _span :: SrcSpan, _varName :: Text }
data Sym =
    SymInt  { _span :: SrcSpan, _int  :: Int  }
  | SymText { _span :: SrcSpan, _text :: Text }
  | SymBool { _span :: SrcSpan, _bool :: Bool }

--------------------------------------------------------------------------------
-- Eq & Ord instances
--------------------------------------------------------------------------------

instance Eq Sym where
  SymInt{_int = i}   == SymInt{_int = i'}   = i == i'
  SymBool{_bool = b} == SymBool{_bool = b'} = b == b'
  SymText{_text = t} == SymText{_text = t'} = t == t'
  _ == _ = False

instance Ord Sym where
  SymInt{_int = i}   `compare` SymInt{_int = i'}   = i `compare` i'
  SymBool{_bool = b} `compare` SymBool{_bool = b'} = b `compare` b'
  SymText{_text = t} `compare` SymText{_text = t'} = t `compare` t'
  SymInt{}  `compare` SymText{} = LT
  SymText{} `compare` SymInt{}  = GT
  SymInt{}  `compare` SymBool{} = LT
  SymBool{} `compare` SymInt{}  = GT
  SymText{} `compare` SymBool{} = LT
  SymBool{} `compare` SymText{} = GT

instance Eq Var where
  Var{_varName = v} == Var{_varName = v'} = v == v'

instance Ord Var where
  Var{_varName = v} `compare` Var{_varName = v'} = v `compare` v'

instance Eq a => Eq (AtomicFormula a) where
  AtomicFormula{_predSym = sym, _terms = ts} ==
    AtomicFormula{_predSym = sym', _terms = ts'} = sym == sym' && ts == ts'

instance Ord a => Ord (AtomicFormula a) where
  atom1 `compare` atom2 =
    (_predSym atom1, _terms atom1) `compare`
    (_predSym atom2, _terms atom2)

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
  _ == _ = False

deriving instance
  (Ord (op 'Nullary), Ord (op 'Unary), Ord (op 'Binary), Ord term)
  => Ord (Subgoal op term)

instance (Eq (Subgoal hop Term)) => Eq (Fact hop) where
  Fact{_head = a} == Fact{_head = a'} = a == a'

deriving instance Ord (Subgoal hop Term) => Ord (Fact hop)

instance ( Eq (Subgoal hop Term)
         , Eq (Subgoal bop Term)
         ) => Eq (Clause hop bop) where
  Clause{_head = h, _body = b} == Clause{_head = h', _body = b'} =
    h == h' && b == b'

deriving instance
  (Ord (Subgoal hop Term), Ord (Subgoal bop Term)) => Ord (Clause hop bop)

instance ( Eq (Subgoal hop Var)
         , Eq (Subgoal bop Term)
         ) => Eq (Query hop bop) where
  Query{_head = h, _body = b} == Query{_head = h', _body = b'} =
    h == h' && b == b'

deriving instance (Ord (Subgoal hop Var), Ord (Subgoal bop Term))
  => Ord (Query hop bop)

instance ( Eq (Fact hop), Eq (Clause hop bop), Eq (Query hop bop)
         ) => Eq (Sentence hop bop) where
  SFact{_fact = f}     == SFact{_fact = f'}     = f == f'
  SQuery{_query = q}   == SQuery{_query = q'}   = q == q'
  SClause{_clause = c} == SClause{_clause = c'} = c == c'
  _                    == _                     = False

deriving instance (Ord (Fact hop), Ord (Clause hop bop), Ord (Query hop bop))
  => Ord (Sentence hop bop)

instance (Eq decl , Eq (Sentence hop bop)) => Eq (Statement decl hop bop) where
  StDeclaration{_declaration = d} == StDeclaration{_declaration = d'} = d == d'
  StSentence{_sentence = s}       == StSentence{_sentence = s'}       = s == s'
  _                               == _                                = False

deriving instance (Ord decl, Ord (Sentence hop bop))
  => Ord (Statement decl hop bop)

--------------------------------------------------------------------------------
-- Template Haskell
--------------------------------------------------------------------------------

makeBaseFunctor ''Subgoal

--------------------------------------------------------------------------------
-- Spannable instanes
--------------------------------------------------------------------------------

instance Spannable Term where
  span TVar{..}  = span _var
  span TSym{..}  = span _sym
  span TWild{..} = _span

instance Spannable (Sentence hop bop) where
  span SFact{..}   = span _fact
  span SClause{..} = span _clause
  span SQuery{..}  = span _query

instance Spannable decl => Spannable (Statement decl hop bop) where
  span StSentence{..}    = span _sentence
  span StDeclaration{..} = span _declaration

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

declarations :: Program decl hop bop -> [ decl ]
declarations pr = (`mapMaybe` _statements pr) $ \case
  StDeclaration decl -> Just decl
  _                  -> Nothing

sentences :: Program decl hop bop -> [ Sentence hop bop ]
sentences pr = (`mapMaybe` _statements pr) $ \case
  StSentence sent -> Just sent
  _               -> Nothing

queries :: Program decl hop bop -> [ Query hop bop ]
queries pr = (`mapMaybe` sentences pr) $ \case
  SQuery query -> Just query
  _            -> Nothing

clauses :: Program decl hop bop -> [ Clause hop bop ]
clauses pr = (`mapMaybe` sentences pr) $ \case
  SClause clause -> Just clause
  _              -> Nothing

facts :: Program decl hop bop -> [ Fact hop ]
facts pr = (`mapMaybe` sentences pr) $ \case
  SFact fact -> Just fact
  _          -> Nothing

termType :: Sym -> TermType
termType SymInt{}  = TTInt
termType SymText{} = TTText
termType SymBool{} = TTBool

operation :: Subgoal op term -> SomeOp op
operation SAtom{}     = NoOp
operation s@SNullOp{} = SomeOp (_nullOp s)
operation s@SUnOp{}   = SomeOp (_unOp s)
operation s@SBinOp{}  = SomeOp (_binOp s)

class HasAtoms a term where
  atoms :: a -> [ AtomicFormula term ]

instance HasAtoms (Program decl hop bop) Term where
  atoms Program{..} = concatMap atoms _statements

instance HasAtoms (Statement decl hop bop) Term where
  atoms StSentence{..} = atoms _sentence
  atoms StDeclaration{..} = []

instance HasAtoms (Sentence hop bop) Term where
  atoms SClause{..} = atoms _clause
  atoms SQuery{..} = atoms _query
  atoms SFact{..} = atoms _fact

instance HasAtoms (Clause hop bop) Term where
  atoms Clause{..} = atoms _head <> atoms _body

instance HasAtoms (Query hop bop) Term where
  atoms Query{..} = (fmap TVar <$> maybe [] atoms _head) <> atoms _body

instance HasAtoms (Fact hop) Term where
  atoms Fact{..} = atoms _head

instance HasAtoms (Subgoal op term) term where
  atoms = cata alg
    where
    alg :: Base (Subgoal op term) [ AtomicFormula term ] -> [ AtomicFormula term ]
    alg SAtomF{..}  = [ _atomF ]
    alg SNullOpF{}  = []
    alg SUnOpF{..}  = _childF
    alg SBinOpF{..} = _child1F ++ _child2F

class HasVariables a where
  variables :: a -> [ Var ]

instance HasVariables (Sentence hop bop) where
  variables SFact{..}   = variables _fact
  variables SClause{..} = variables _clause
  variables SQuery{..}  = variables _query

instance HasVariables (Clause hop bop) where
  variables Clause{..} = variables _head ++ variables _body

instance HasVariables (Query hop bop) where
  variables Query{..} = variables _body

instance HasVariables (Fact hop) where
  variables Fact{..} = variables _head

instance HasVariables (AtomicFormula t) => HasVariables (Subgoal op t) where
  variables = nub . cata varAlg
    where
    varAlg (SAtomF _ atom@AtomicFormula{}) = variables atom
    varAlg (SNullOpF _ _)                  = []
    varAlg (SUnOpF _ _ vars)               = vars
    varAlg (SBinOpF _ _ vars1 vars2)       = vars1 ++ vars2

instance HasVariables (AtomicFormula Term) where
  variables AtomicFormula{..} =
    mapMaybe (\case {TVar{..} -> Just _var; _ -> Nothing}) _terms

instance HasVariables (AtomicFormula Var) where
  variables AtomicFormula{..} = _terms

instance HasVariables (AtomicFormula Sym) where
  variables AtomicFormula{..} = []

--------------------------------------------------------------------------------
-- IsLabel instances
--------------------------------------------------------------------------------

instance IsLabel "_predSym" (AtomicFormula a -> PredicateSymbol) where
  fromLabel AtomicFormula{..} = _predSym

instance IsLabel "_head" (Fact hop -> Subgoal hop Term) where
  fromLabel Fact{..} = _head

instance IsLabel "_head" (Clause hop bop -> Subgoal hop Term) where
  fromLabel Clause{..} = _head

instance IsLabel "_head" (Query hop bop -> Maybe (Subgoal hop Var)) where
  fromLabel Query{..} = _head
