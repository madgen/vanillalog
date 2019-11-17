{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Language.Vanillalog.Generic.DSL
  ( GenericDatalogT
  , genDatalogT
    -- * Predicate maker
  , mkPredicate0
  , mkPredicate1
  , mkPredicate2
  , mkPredicate3
  , mkPredicate4
  , mkPredicate5
  , mkPredicate6
    -- * Term constructor
  , var
  , text
  , int
    -- * Sentence constructor
  , (|-)
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

mkPredicate0 :: PredicateSymbol -> Subgoal op Term
mkPredicate0 predSym = atom predSym [ ]

mkPredicate1 :: PredicateSymbol -> Term -> Subgoal op Term
mkPredicate1 predSym t = atom predSym [ t ]

mkPredicate2 :: PredicateSymbol -> (Term,Term) -> Subgoal op Term
mkPredicate2 predSym (t1,t2) = atom predSym [ t1, t2 ]

mkPredicate3 :: PredicateSymbol -> (Term,Term,Term) -> Subgoal op Term
mkPredicate3 predSym (t1,t2,t3) = atom predSym [ t1, t2, t3 ]

mkPredicate4 :: PredicateSymbol -> (Term,Term,Term,Term) -> Subgoal op Term
mkPredicate4 predSym (t1,t2,t3,t4) = atom predSym [ t1, t2, t3, t4 ]

mkPredicate5 :: PredicateSymbol -> (Term,Term,Term,Term,Term) -> Subgoal op Term
mkPredicate5 predSym (t1,t2,t3,t4,t5) = atom predSym [ t1, t2, t3, t4, t5 ]

mkPredicate6 :: PredicateSymbol -> (Term,Term,Term,Term,Term,Term) -> Subgoal op Term
mkPredicate6 predSym (t1,t2,t3,t4,t5,t6) = atom predSym [ t1, t2, t3, t4, t5, t6 ]

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

infix 1 |-
(|-) :: Monad m
     => Subgoal hop Term -> Subgoal bop Term -> GenericDatalogT decl hop bop m
head |- body = modify ((StSentence $ SClause $ Clause NoSpan head body) :)

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
