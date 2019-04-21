{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
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
import qualified Data.Text as Text
import qualified Data.Vector.Sized as V

import qualified Language.Exalog.Core as E
import qualified Language.Exalog.Logger as L
import qualified Language.Exalog.Tuples as T
import qualified Language.Exalog.Relation as R
import           Language.Exalog.SrcLoc (span)

import           Language.Vanillalog.Generic.AST

class Compilable a where
  type Output a
  compile :: a -> Output a

instance ( Compilable (Clause hop bop)
         , Compilable (Query hop bop)
         , Compilable (Fact hop)
         ) => Compilable (Program Void hop bop) where
  type Output (Program Void hop bop) =
    L.Logger (E.Program 'E.ABase, R.Solution 'E.ABase)

  compile Program{..} = action
    where
    action = do
      edb <- traverse compile facts
      compiledClauses <- traverse compile clauses
      compiledQueries <- traverse compile queries
      return
        ( E.Program
            { annotation = E.ProgABase _span
            , clauses    = compiledClauses ++ compiledQueries
            , queryPreds = queryPreds compiledQueries
            }
        , R.fromList edb
        )

    clauses = [ _clause s | StSentence s@SClause{} <- _statements ]
    facts   = [ _fact   s | StSentence s@SFact{}   <- _statements ]
    queries = [ _query  s | StSentence s@SQuery{}  <- _statements ]

    queryPreds = map (E.predicateBox . E.head)

instance Compilable (Fact hop) where
  type Output (Fact hop) = L.Logger (R.Relation E.ABase)
  compile Fact{_head = sub,..} = case sub of
    SAtom{_atom = AtomicFormula{..}} ->
      withSomeSing (fromInteger . toInteger . length $ _terms) $
        \(arity :: SNat n) -> do
          syms <- traverse castToSym _terms
          withKnownNat arity $ do
            tuples <-
              case V.fromListN @n _terms of
                Just vec -> do
                  symVec <- traverse castToSym vec
                  pure $ T.fromList [ map compile symVec ]
                Nothing -> L.scream (Just _span)
                  "length of terms is not the length of terms."
            pure $ R.Relation
              E.Predicate
                { annotation = E.PredABase _span
                , fxSym = flatten _predSym
                , arity = arity
                , nature = E.Logical
                }
              tuples
    _ -> L.scream (Just _span) "The head is not ready for compilation."
    where
    castToSym :: Term -> L.Logger Sym
    castToSym TVar{_var = Var{..}} = L.scold (Just _span)
      "Facts cannot have variables. Range restriction is violated."
    castToSym TSym{..} = pure _sym

instance (ClosureCompilable bop) => Compilable (Clause hop bop) where
  type Output (Clause hop bop) = L.Logger (E.Clause 'E.ABase)
  compile Clause{..} = E.Clause (E.ClABase _span)
                   <$> headCompile _head
                   <*> bodyCompile _body

instance Compilable (Clause hop bop) => Compilable (Query hop bop) where
  type Output (Query hop bop) = L.Logger (E.Clause 'E.ABase)
  compile Query{..} = case _head of
    Just sub -> compile (Clause
      {_head = SAtom { _span = span sub
                     , _atom = TVar <$> _atom sub
                     }
      ,..} :: Clause hop bop)
    Nothing -> L.scream (Just _span) "Unnamed query found during compilation."

headCompile :: Subgoal op Term -> L.Logger (E.Literal 'E.ABase)
headCompile SAtom{..} = compile _atom
headCompile s = L.scream (Just $ span s) "Head is not ready for compilation."

bodyCompile :: forall op. ClosureCompilable op
            => Subgoal op Term -> L.Logger (E.Body 'E.ABase)
bodyCompile = para alg
  where
  alg :: Base (Subgoal op Term) (Subgoal op Term, L.Logger (E.Body 'E.ABase))
      -> L.Logger (E.Body 'E.ABase)
  alg (SAtomF  _ atom) = (NE.:| []) <$> compile atom
  alg (SUnOpF  _ op (ch,m)) = cCompile =<< (CUnary op . (ch,) <$> m)
  alg (SBinOpF _ op (ch1,m1) (ch2,m2)) =
    cCompile =<< liftA2 (CBinary op) ((ch1,) <$> m1) ((ch2,) <$> m2)

data Closure op =
    CUnary  (op 'Unary)  (Subgoal op Term, E.Body 'E.ABase)
  | CBinary (op 'Binary) (Subgoal op Term, E.Body 'E.ABase)
                         (Subgoal op Term, E.Body 'E.ABase)

class ClosureCompilable op where
  cCompile :: Closure op -> L.Logger (E.Body 'E.ABase)

flatten :: PredicateSymbol -> Text
flatten (PredicateSymbol names) = Text.intercalate ":" $ reverse names

instance Compilable (AtomicFormula Term) where
  type Output (AtomicFormula Term) = L.Logger (E.Literal 'E.ABase)
  compile AtomicFormula{..} =
    withSomeSing (fromInteger . toInteger $ length _terms) $
      \(arity :: SNat n) -> do
        terms <- withKnownNat arity $
          case V.fromListN @n _terms of
            Just vec -> pure $ fmap compile vec
            Nothing -> L.scream (Just _span)
              "Length of terms is not the length of terms."
        pure $ E.Literal
          { annotation = E.LitABase _span
          , polarity   = E.Positive
          , predicate  = E.Predicate
              { annotation = E.PredABase _span
              , fxSym      = flatten _predSym
              , arity      = arity
              , nature     = E.Logical }
          , terms = terms
          }

instance Compilable Term where
  type Output Term = E.Term
  compile TVar{ _var = v } = E.TVar $ compile v
  compile TSym{ _sym = s } = E.TSym $ compile s
  compile TWild{}          = E.TWild

instance Compilable Var where
  type Output Var = E.Var
  compile (Var _ v) = E.Var v

instance Compilable Sym where
  type Output Sym = E.Sym
  compile (SymInt  _ i)  = E.SymInt i
  compile (SymText _ bs) = E.SymText bs
  compile (SymBool _ b)  = E.SymBool b
