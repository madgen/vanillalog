{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Vanillalog.Generic.Compiler
  ( compile
  , Closure(..)
  , ClosureCompilable(..)
  ) where

import Protolude hiding (head)

import           Data.Functor.Foldable (Base, para)
import qualified Data.List.NonEmpty as NE
import           Data.Singletons (withSomeSing)
import           Data.Singletons.TypeLits (SNat, withKnownNat)
import qualified Data.Vector.Sized as V

import qualified Language.Exalog.Core as E
import qualified Language.Exalog.Relation as R
import qualified Language.Exalog.Tuples as T

import           Language.Vanillalog.Generic.AST

class Compilable a where
  type Output a
  compile :: a -> Output a

instance ClosureCompilable op => Compilable (Program op) where
  type Output (Program op) = (E.Program 'E.ABase, R.Solution 'E.ABase)
  compile (Program sentences) =
    ( E.Program
        { annotation = E.ProgABase
        , clauses    = map compile clauses ++ map compile queries
        , queryPreds = queryPreds
        }
    , R.fromList $ map compile facts
    )
    where
    clauses = [ clause | SClause clause <- sentences ]
    facts   = [ fact   | SFact fact     <- sentences ]
    queries = [ query  | SQuery query   <- sentences ]

    clausifiedQueries = map compile queries
    queryPreds = map (E.predicateBox . E.head) $ clausifiedQueries

instance Compilable Fact where
  type Output Fact = R.Relation E.ABase
  compile (Fact (AtomicFormula name terms)) =
    withSomeSing (fromInteger . toInteger . length $ terms) $
      \(arity :: SNat n) ->
        withKnownNat arity $
          R.Relation
            E.Predicate
              { annotation = E.PredABase
              , fxSym = name
              , arity = arity
              , nature = E.Logical
              }
            (T.fromList $
              case V.fromListN @n terms of
                Just vec -> [ fromTerm . compile <$> vec ]
                Nothing -> panic
                  "Impossible: length of terms is not the length of terms.")
    where
    fromTerm :: E.Term -> E.Sym
    fromTerm (E.TVar _) = panic "Facts cannot have variables."
    fromTerm (E.TSym s) = s


instance ClosureCompilable op => Compilable (Clause op) where
  type Output (Clause op) = E.Clause 'E.ABase
  compile (Clause head body) = E.Clause
    { annotation = E.ClABase
    , head = compile head
    , body = compile body
    }

instance ClosureCompilable op => Compilable (Query op) where
  type Output (Query op) = E.Clause 'E.ABase
  compile (Query (Just head) body) = compile (Clause head body)
  compile _ = panic "Impossible: Unnamed query found during compilation."

instance ClosureCompilable op => Compilable (Subgoal op) where
  type Output (Subgoal op) = E.Body 'E.ABase

  compile = para alg
    where
    alg :: Base (Subgoal op) (Subgoal op, Output (Subgoal op))
        -> Output (Subgoal op)
    alg (SAtomF atom)            = compile atom NE.:| []
    alg (SUnOpF  op rec)         = cCompile (CUnary  op rec)
    alg (SBinOpF op rec1 rec2)   = cCompile (CBinary op rec1 rec2)

data Closure op =
    CUnary  (op 'Unary)  (Subgoal op, Output (Subgoal op))
  | CBinary (op 'Binary) (Subgoal op, Output (Subgoal op))
                         (Subgoal op, Output (Subgoal op))

class ClosureCompilable op where
  cCompile :: Closure op -> E.Body 'E.ABase

instance Compilable AtomicFormula where
  type Output AtomicFormula = E.Literal 'E.ABase
  compile (AtomicFormula name terms) =
    withSomeSing (fromInteger . toInteger $ length terms) $
      \(arity :: SNat n) ->
        E.Literal
          { annotation = E.LitABase
          , polarity   = E.Positive
          , predicate  = E.Predicate
              { annotation = E.PredABase
              , fxSym      = name
              , arity      = arity
              , nature     = E.Logical }
          , terms =
              withKnownNat arity $
                case V.fromListN @n terms of
                  Just vec -> fmap compile vec
                  Nothing -> panic
                    "Impossible: length of terms is not the length of terms."
          }

instance Compilable Term where
  type Output Term = E.Term
  compile (TVar v) = E.TVar $ compile v
  compile (TSym s) = E.TSym $ compile s

instance Compilable Var where
  type Output Var = E.Var
  compile (Var v) = E.Var v

instance Compilable Sym where
  type Output Sym = E.Sym
  compile (SymInt i)   = E.SymInt i
  compile (SymText bs) = E.SymText bs
  compile (SymBool b)  = E.SymBool b
