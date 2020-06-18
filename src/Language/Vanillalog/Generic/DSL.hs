{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}

module Language.Vanillalog.Generic.DSL
  ( GenericDatalogT
  , genDatalogT
    -- * Predicate maker
  , predicate
    -- * Term constructor
  , var
  , text
  , int
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

type GenericDatalogT decl hop bop m = StateT [ Statement decl hop bop ] m ()

genDatalogT :: Monad m
            => GenericDatalogT decl hop bop m -> m (Program decl hop bop)
genDatalogT = (Program NoSpan <$>) . (`execStateT` [])

infix 1 -|
(-|) :: Monad m
     => Subgoal hop Term -> Subgoal bop Term -> GenericDatalogT decl hop bop m
head -| body = modify ((StSentence $ SClause $ Clause NoSpan head body) :)

data SKind = Q | F
data StatementKind :: SKind -> Type where
  Query :: StatementKind 'Q
  Fact  :: StatementKind 'F

infix 1 |>
class Statementable typ op hop bop | typ hop bop -> op where
  (|>) :: Monad m
       => StatementKind typ -> Subgoal op Term -> GenericDatalogT decl hop bop m

instance Statementable 'Q op hop op where
  Language.Vanillalog.Generic.DSL.Query |> body = modify
    ((StSentence $ SQuery $ GA.Query NoSpan Nothing body) :)

instance Statementable 'F op op bop where
  Language.Vanillalog.Generic.DSL.Fact |> head = modify
    ((StSentence $ SFact $ GA.Fact NoSpan head) :)
