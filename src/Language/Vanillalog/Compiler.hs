{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Vanillalog.Compiler
  ( compile
  ) where

import Protolude hiding (head)

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Functor.Foldable (Base, para)
import qualified Data.List.NonEmpty as NE
import           Data.Singletons (withSomeSing)
import           Data.Singletons.TypeLits (SNat, withKnownNat)
import qualified Data.Vector.Sized as V

import qualified Language.Exalog.Core as E
import qualified Language.Exalog.Relation as R
import qualified Language.Exalog.Tuples as T

import Language.Vanillalog.AST

class Compilable a where
  type Output a
  compile :: a -> Output a

instance Compilable Program where
  type Output Program = (E.Program 'E.ABase, R.Solution 'E.ABase)
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
              , fxSym = decodeUtf8 . BS.toStrict $ name
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


instance Compilable Clause where
  type Output Clause = E.Clause 'E.ABase
  compile (Clause head body) = E.Clause
    { annotation = E.ClABase
    , head = compile head
    , body = compile body
    }

instance Compilable Query where
  type Output Query = E.Clause 'E.ABase
  compile (Query (Just head) body) = compile (Clause head body)
  compile _ = panic "Impossible: Unnamed query found during compilation."

instance Compilable VanillaSubgoal where
  type Output VanillaSubgoal = NE.NonEmpty (E.Literal 'E.ABase)
  compile = para alg
    where
    alg :: Base VanillaSubgoal (VanillaSubgoal, Output VanillaSubgoal)
        -> Output VanillaSubgoal
    alg (SAtomF atom) = compile atom NE.:| []
    alg (SNegF rec)
      | (SAtom{}, core NE.:| []) <- rec =
        core { E.polarity = E.Negative } NE.:| []
      | otherwise = panic
         "Impossible: Negation over non-atoms should be eliminated at this point."
    alg (SConjF (_,core1) (_,core2)) = core1 `append` core2
    alg (SDisjF _ _) =
      panic "Impossible: Disjunctions should be eliminated at this point."

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
              , fxSym      = decodeUtf8 . BS.toStrict $ name
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
  compile (Var v) = E.Var . decodeUtf8 . BS.toStrict $ v

instance Compilable Sym where
  type Output Sym = E.Sym
  compile (SymInt i)   = E.SymInt i
  compile (SymText bs) = E.SymText . decodeUtf8 . BS.toStrict $ bs
  compile (SymBool b)  = E.SymBool b

-- Util

append :: NE.NonEmpty a -> NE.NonEmpty a -> NE.NonEmpty a
append (a NE.:| as) (a' NE.:| as') = a NE.:| as ++ a' : as'
