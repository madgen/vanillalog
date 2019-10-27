{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Vanillalog.Foreign
  ( F.FFILookupStatus
  , lookupForeignVanillalog
  ) where

import Protolude

import Data.Type.Equality ((:~:)(Refl))
import GHC.TypeLits (sameNat)

import qualified Data.Map.Strict as M

import qualified Language.Exalog.Core as E

import qualified Language.Vanillalog.Generic.Foreign as F

foreignTable :: M.Map E.PredicateSymbol E.SomeForeignFunc
foreignTable = M.fromList
  -- Arithmetic
  [ ("add",                         E.SFF F.add)
  , ("subtract",                    E.SFF F.subtract)
  , ("lt",                          E.SFF F.lt)
  , ("lte",                         E.SFF F.lte)
  , ("gt",                          E.SFF F.gt)
  , ("gte",                         E.SFF F.gte)
  -- Unification
  , ("unify_int",                   E.SFF F.unifyInt)
  , ("unify_text",                  E.SFF F.unifyText)
  , ("unify_bool",                  E.SFF F.unifyBool)
  -- IO
  , ("csv1",                        E.SFF F.csv1)
  , ("csv2",                        E.SFF F.csv2)
  , ("csv3",                        E.SFF F.csv3)
  , ("csv4",                        E.SFF F.csv4)
  ]

lookupForeignVanillalog
  :: forall p n . KnownNat n
  => E.PredicateSymbol -> p n -> F.FFILookupStatus n
lookupForeignVanillalog predSym _ =
  case predSym `M.lookup` foreignTable of
    Nothing -> F.NotFound
    Just (E.SFF (foreignFunc :: E.ForeignFunc m)) ->
      case Proxy @n `sameNat` Proxy @m of
        Just Refl -> F.Found foreignFunc
        Nothing   -> F.WrongArity
