{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Vanillalog.Generic.Compiler
  ( compile
  , Closure(..)
  , ClosureCompilable(..)
  ) where

import Protolude hiding (head)

import GHC.TypeLits (sameNat)

import           Data.Functor.Foldable (Base, para)
import           Data.List (nub)
import qualified Data.List.NonEmpty as NE
import           Data.Singletons (withSomeSing)
import           Data.Singletons.TypeLits (SNat, withKnownNat)
import qualified Data.Vector.Sized as V

import qualified Language.Exalog.Core as E
import qualified Language.Exalog.Logger as L
import qualified Language.Exalog.KnowledgeBase.Class as KB
import qualified Language.Exalog.KnowledgeBase.Knowledge as KB
import qualified Language.Exalog.KnowledgeBase.Set as KB
import           Language.Exalog.SrcLoc (span)
import           Language.Exalog.Pretty (pp)

import           Language.Vanillalog.Generic.AST

class Compilable a where
  type Output a
  compile :: a -> Output a

instance ( Compilable (Clause hop bop)
         , Compilable (Query hop bop)
         , Compilable (Fact hop)
         ) => Compilable (Program Void hop bop) where
  type Output (Program Void hop bop) =
    L.Logger (E.Program 'E.ABase, KB.Set 'E.ABase)

  compile Program{..} = do
    edb             <- traverse compile facts'
    compiledClauses <- traverse compile clauses'
    compiledQueries <- traverse compile queries'
    return
      ( E.Program
          { _annotation = E.ProgABase _span
          , _strata     = [ E.Stratum $ compiledClauses ++ compiledQueries ]
          , _queries    = queryPreds compiledQueries
          }
      , KB.fromList edb
      )

    where
    clauses' = [ _clause s | StSentence s@SClause{} <- _statements ]
    facts'   = [ _fact   s | StSentence s@SFact{}   <- _statements ]
    queries' = [ _query  s | StSentence s@SQuery{}  <- _statements ]

    queryPreds = nub . sort . map (E.predicateBox . E._head)

instance Compilable (Fact hop) where
  type Output (Fact hop) = L.Logger (KB.Knowledge 'E.ABase)
  compile Fact{_head = sub,..} = case sub of
    SAtom{_atom = AtomicFormula{..}} ->
      withSomeSing (fromInteger . toInteger . length $ _terms) $
        \(arity :: SNat n) ->
          withKnownNat arity $ do
            tuple <-
              case V.fromListN @n _terms of
                Just vec -> do
                  symVec <- traverse castToSym vec
                  pure $ map compile symVec
                Nothing -> L.scream _span
                  "length of terms is not the length of terms."
            pure $ KB.Knowledge E.KnowABase
              E.Predicate
                { _annotation = E.PredABase _span
                , _predSym    = _predSym
                , _arity      = arity
                , _nature     = E.Logical
                }
              tuple
    _ -> L.scream _span "The head is not ready for compilation."
    where
    castToSym :: Term -> L.Logger Sym
    castToSym TVar{_var = Var{..}} = L.scold _span
      "Facts cannot have variables. Range restriction is violated."
    castToSym TWild{}              = L.scold _span
      "Facts cannot have wildcards."
    castToSym TSym{..}             = pure _sym

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
    Nothing -> L.scream _span "Unnamed query found during compilation."

headCompile :: Subgoal op Term -> L.Logger (E.Literal 'E.ABase)
headCompile SAtom{..} = compile _atom
headCompile s = L.scream (span s) "Head is not ready for compilation."

bodyCompile :: forall op. ClosureCompilable op
            => Subgoal op Term -> L.Logger (E.Body 'E.ABase)
bodyCompile = para alg
  where
  alg :: Base (Subgoal op Term) (Subgoal op Term, L.Logger (E.Body 'E.ABase))
      -> L.Logger (E.Body 'E.ABase)
  alg (SAtomF  _ atom)                 = (NE.:| []) <$> compile atom
  alg (SUnOpF  _ op (ch,m))            = cCompile =<< (CUnary op . (ch,) <$> m)
  alg (SBinOpF _ op (ch1,m1) (ch2,m2)) =
    cCompile =<< liftA2 (CBinary op) ((ch1,) <$> m1) ((ch2,) <$> m2)
  alg (SNullOpF s _)                   =
    L.scream s "Nullary operators cannot be compiled."

data Closure op =
    CUnary  (op 'Unary)  (Subgoal op Term, E.Body 'E.ABase)
  | CBinary (op 'Binary) (Subgoal op Term, E.Body 'E.ABase)
                         (Subgoal op Term, E.Body 'E.ABase)

class ClosureCompilable op where
  cCompile :: Closure op -> L.Logger (E.Body 'E.ABase)

instance Compilable (AtomicFormula Term) where
  type Output (AtomicFormula Term) = L.Logger (E.Literal 'E.ABase)
  compile AtomicFormula{..} =
    withSomeSing (fromInteger . toInteger $ length _terms) $
      \(arity :: SNat n) ->
        withKnownNat arity $ do
          terms <-
            case V.fromListN @n _terms of
              Just vec -> pure $ fmap compile vec
              Nothing -> L.scream _span
                "Length of terms is not the length of terms."

          nature <-
            case _nature of
              Just (E.SFF (foreignFunc :: E.ForeignFunc m)) ->
                case Proxy @n `sameNat` Proxy @m of
                  Just Refl -> pure $ E.Extralogical foreignFunc
                  Nothing -> L.scold _span $
                       "The foreign function has arity "
                    <> pp (fromIntegral @_ @Int $ natVal (Proxy @m))
                    <> ", but you've given "
                    <> pp (fromIntegral @_ @Int $ natVal arity) <> " arguments."
              Nothing -> pure E.Logical

          pure $ E.Literal
            { _annotation = E.LitABase _span
            , _polarity   = E.Positive
            , _predicate  = E.Predicate
                { _annotation = E.PredABase _span
                , _predSym    = _predSym
                , _arity      = arity
                , _nature     = nature }
            , _terms = terms
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
