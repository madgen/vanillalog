{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
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
import qualified Language.Vanillalog.Generic.Logger as L

class Compilable a where
  type Output a
  compile :: a -> Output a

instance ClosureCompilable op => Compilable (Program op) where
  type Output (Program op) =
    Either [ Text ] (E.Program 'E.ABase, R.Solution 'E.ABase)
  compile (Program sentences) = L.runLogger action
    where
    action = do
      edb <- traverse compile facts
      compiledClauses <- traverse compile clauses
      compiledQueries <- traverse compile queries
      return $
        ( E.Program
            { annotation = E.ProgABase
            , clauses    = compiledClauses ++ compiledQueries
            , queryPreds = queryPreds compiledQueries
            }
        , R.fromList edb
        )

    clauses = [ clause | SClause clause <- sentences ]
    facts   = [ fact   | SFact fact     <- sentences ]
    queries = [ query  | SQuery query   <- sentences ]

    queryPreds = map (E.predicateBox . E.head)

instance Compilable Fact where
  type Output Fact = L.LoggerM (R.Relation E.ABase)
  compile (Fact (AtomicFormula name terms)) =
    withSomeSing (fromInteger . toInteger . length $ terms) $
      \(arity :: SNat n) ->
        withKnownNat arity $ do
          tuples <-
            case V.fromListN @n terms of
              Just vec -> T.fromList . pure <$> traverse (fromTerm . compile) vec
              Nothing -> L.scream
                "Impossible: length of terms is not the length of terms."
          pure $ R.Relation
            E.Predicate
              { annotation = E.PredABase
              , fxSym = name
              , arity = arity
              , nature = E.Logical
              }
            tuples
    where
    fromTerm :: E.Term -> L.LoggerM E.Sym
    fromTerm (E.TVar _) = L.scream "Facts cannot have variables."
    fromTerm (E.TSym s) = pure s


instance ClosureCompilable op => Compilable (Clause op) where
  type Output (Clause op) = L.LoggerM (E.Clause 'E.ABase)
  compile (Clause head body) =
    E.Clause E.ClABase <$> compile head <*> compile body

instance ClosureCompilable op => Compilable (Query op) where
  type Output (Query op) = L.LoggerM (E.Clause 'E.ABase)
  compile (Query (Just head) body) = compile (Clause head body)
  compile _ = L.scream "Impossible: Unnamed query found during compilation."

instance ClosureCompilable op => Compilable (Subgoal op) where
  type Output (Subgoal op) = L.LoggerM (E.Body 'E.ABase)

  compile = para alg
    where
    alg :: Base (Subgoal op) (Subgoal op, Output (Subgoal op))
        -> Output (Subgoal op)
    alg (SAtomF atom) = (NE.:| []) <$> compile atom
    alg (SUnOpF  op (ch,m)) = cCompile =<< (CUnary op . (ch,) <$> m)
    alg (SBinOpF op (ch1,m1) (ch2,m2)) =
      cCompile =<< liftA2 (CBinary op) ((ch1,) <$> m1) ((ch2,) <$> m2)

data Closure op =
    CUnary  (op 'Unary)  (Subgoal op, E.Body 'E.ABase)
  | CBinary (op 'Binary) (Subgoal op, E.Body 'E.ABase)
                         (Subgoal op, E.Body 'E.ABase)

class ClosureCompilable op where
  cCompile :: Closure op -> L.LoggerM (E.Body 'E.ABase)

instance Compilable AtomicFormula where
  type Output AtomicFormula = L.LoggerM (E.Literal 'E.ABase)
  compile (AtomicFormula name terms) =
    withSomeSing (fromInteger . toInteger $ length terms) $
      \(arity :: SNat n) -> do
        terms <- withKnownNat arity $
          case V.fromListN @n terms of
            Just vec -> pure $ fmap compile vec
            Nothing ->
              L.scream "Impossible: length of terms is not the length of terms."
        pure $ E.Literal
          { annotation = E.LitABase
          , polarity   = E.Positive
          , predicate  = E.Predicate
              { annotation = E.PredABase
              , fxSym      = name
              , arity      = arity
              , nature     = E.Logical }
          , terms = terms
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
