{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Vanillalog.Foreign
  ( foreignTable
  ) where

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
  , ("read_csv1",                   E.SFF F.readCSV1)
  , ("read_csv2",                   E.SFF F.readCSV2)
  , ("read_csv3",                   E.SFF F.readCSV3)
  , ("read_csv4",                   E.SFF F.readCSV4)
  , ("append_csv1",                 E.SFF F.appendCSV1)
  , ("append_csv2",                 E.SFF F.appendCSV2)
  , ("append_csv3",                 E.SFF F.appendCSV3)
  , ("append_csv4",                 E.SFF F.appendCSV4)
  ]
