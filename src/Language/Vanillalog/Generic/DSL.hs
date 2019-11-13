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
  ( GenericDatalog
    -- * Predicate maker
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
  , (.)
  , voila
  ) where

import qualified Protolude as P
import           Protolude hiding (head, (.), (-))

import Data.String (IsString(..))

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

mkPredicate1 :: PredicateSymbol -> Term -> Subgoal op Term
mkPredicate1 predSym t = atom predSym [ t ]

mkPredicate2 :: PredicateSymbol -> Term -> Term -> Subgoal op Term
mkPredicate2 predSym t1 t2 = atom predSym [ t1, t2 ]

mkPredicate3 :: PredicateSymbol -> Term -> Term -> Term -> Subgoal op Term
mkPredicate3 predSym t1 t2 t3 = atom predSym [ t1, t2, t3 ]

mkPredicate4 :: PredicateSymbol -> Term -> Term -> Term -> Term -> Subgoal op Term
mkPredicate4 predSym t1 t2 t3 t4 = atom predSym [ t1, t2, t3, t4 ]

mkPredicate5 :: PredicateSymbol -> Term -> Term -> Term -> Term -> Term -> Subgoal op Term
mkPredicate5 predSym t1 t2 t3 t4 t5 = atom predSym [ t1, t2, t3, t4, t5 ]

mkPredicate6 :: PredicateSymbol -> Term -> Term -> Term -> Term -> Term -> Term -> Subgoal op Term
mkPredicate6 predSym t1 t2 t3 t4 t5 t6 = atom predSym [ t1, t2, t3, t4, t5, t6 ]

var :: Text -> Term
var name = TVar $ Var NoSpan name

text :: Text -> Term
text literal = TSym $ SymText NoSpan literal

int :: Int -> Term
int literal = TSym $ SymInt NoSpan literal

instance IsString Term where
  fromString str = TSym $ SymText NoSpan (fromString str)

type GenericDatalog decl hop bop = [ Statement decl hop bop ] -> [ Statement decl hop bop ]

infixr 0 .
(.) :: GenericDatalog decl hop bop -> GenericDatalog decl hop bop -> GenericDatalog decl hop bop
d1 . d2 = d1 P.. d2

infix 1 |-
(|-) :: Subgoal hop Term -> Subgoal bop Term -> GenericDatalog decl hop bop
head |- body = ((StSentence $ SClause $ Clause NoSpan head body) :)

data SKind = Q | F
data StatementKind :: SKind -> Type where
  Query :: StatementKind 'Q
  Fact  :: StatementKind 'F

infix 1 |>
class Statementable typ op hop bop | typ hop bop -> op where
  (|>) :: StatementKind typ -> Subgoal op Term -> GenericDatalog decl hop bop

instance Statementable 'Q op hop op where
  Language.Vanillalog.Generic.DSL.Query |> body =
    ((StSentence $ SQuery $ GA.Query NoSpan Nothing body) :)

instance Statementable 'F op op bop where
  Language.Vanillalog.Generic.DSL.Fact |> head =
    ((StSentence $ SFact $ GA.Fact NoSpan head) :)

voila :: GenericDatalog decl hop bop
voila = const []
