{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}

module Language.Vanillalog.Generic.DSL
  ( GenericDatalog
  , genDatalog
    -- * Predicate maker
  , predicate
    -- * Term constructor
  , var
  , text
  , int
  , wildcard
    -- * Sentence constructor
  , (-|)
  , (|>)
  , StatementKind(..)
  ) where

import Protolude hiding (head)

import Language.Exalog.SrcLoc
import Language.Exalog.Core (PredicateSymbol)

import Language.Vanillalog.Generic.AST as GA

atom :: PredicateSymbol -> [ Term ] -> Subgoal op Term
atom predSym terms = SAtom NoSpan $ AtomicFormula
  { _span = NoSpan
  , _predSym = predSym
  , _nature = Nothing
  , _terms = terms
  }

class Predicatable a where
  predicate :: PredicateSymbol -> a -> Subgoal op Term

instance Predicatable () where
  predicate predSym () = atom predSym [ ]

instance Predicatable Term where
  predicate predSym term = atom predSym [ term ]

instance Predicatable (Term, Term) where
  predicate predSym (t1,t2) = atom predSym [ t1, t2 ]

instance Predicatable (Term, Term, Term) where
  predicate predSym (t1,t2,t3) = atom predSym [ t1, t2, t3 ]

instance Predicatable (Term, Term, Term, Term) where
  predicate predSym (t1,t2,t3,t4) = atom predSym [ t1, t2, t3, t4 ]

instance Predicatable (Term, Term, Term, Term, Term) where
  predicate predSym (t1,t2,t3,t4,t5) = atom predSym [ t1, t2, t3, t4, t5 ]

instance Predicatable (Term, Term, Term, Term, Term, Term) where
  predicate predSym (t1,t2,t3,t4,t5,t6) = atom predSym [ t1, t2, t3, t4, t5, t6 ]

var :: Text -> Term
var name = TVar $ Var NoSpan name

text :: Text -> Term
text literal = TSym $ SymText NoSpan literal

int :: Int -> Term
int literal = TSym $ SymInt NoSpan literal

wildcard :: Term
wildcard = TWild NoSpan

type GenericDatalog decl hop bop = State [ Statement decl hop bop ] ()

genDatalog :: GenericDatalog decl hop bop -> Program decl hop bop
genDatalog = Program NoSpan . (`execState` [])

infix 1 -|
(-|) :: Subgoal hop Term -> Subgoal bop Term -> GenericDatalog decl hop bop
head -| body = modify ((StSentence $ SClause $ Clause NoSpan head body) :)

data SKind = Q | F
data StatementKind :: SKind -> Type where
  Query :: StatementKind 'Q
  Fact  :: StatementKind 'F

infix 1 |>
class Statementable ty op hop bop | ty hop bop -> op where
  (|>) :: StatementKind ty -> Subgoal op Term -> GenericDatalog decl hop bop

instance Statementable 'Q op hop op where
  Language.Vanillalog.Generic.DSL.Query |> body = modify
    ((StSentence $ SQuery $ GA.Query NoSpan Nothing body) :)

instance Statementable 'F op op bop where
  Language.Vanillalog.Generic.DSL.Fact |> head = modify
    ((StSentence $ SFact $ GA.Fact NoSpan head) :)
