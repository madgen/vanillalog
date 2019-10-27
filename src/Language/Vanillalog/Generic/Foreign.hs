{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-| A small library of foreign functions. -}
module Language.Vanillalog.Generic.Foreign
  ( -- * Arithmetic
    add
  , subtract
  , lt
  , lte
  , gt
  , gte
    -- * Unification
  , unifyInt
  , unifyText
  , unifyBool
    -- * IO
  , csv1
  , csv2
  , csv3
  , csv4
  ) where

import Protolude hiding (subtract)

import Control.Monad.Trans.Except (except, withExceptT)

import qualified Data.Csv as CSV
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Vector as V

import Language.Exalog.Core
import Language.Exalog.ForeignFunction

add :: ForeignFunc 3
add = liftFunction ((+) :: Int -> Int -> Int)

subtract :: ForeignFunc 3
subtract = liftFunction ((-) :: Int -> Int -> Int)

lt :: ForeignFunc 2
lt = liftPredicate ((<) :: Int -> Int -> Bool)

lte :: ForeignFunc 2
lte = liftPredicate ((<=) :: Int -> Int -> Bool)

gt :: ForeignFunc 2
gt = liftPredicate ((>) :: Int -> Int -> Bool)

gte :: ForeignFunc 2
gte = liftPredicate ((>=) :: Int -> Int -> Bool)

unifyInt :: ForeignFunc 2
unifyInt = liftPredicate ((==) :: Int -> Int -> Bool)

unifyText :: ForeignFunc 2
unifyText = liftPredicate ((==) :: Text -> Text -> Bool)

unifyBool :: ForeignFunc 2
unifyBool = liftPredicate ((==) :: Bool -> Bool -> Bool)

csv1 :: ForeignFunc 2
csv1 = liftFunctionME (fmap (CSV.fromOnly <$>) <$> srcToResults @(CSV.Only Text))

csv2 :: ForeignFunc 3
csv2 = liftFunctionME (srcToResults @(Text,Text))

csv3 :: ForeignFunc 4
csv3 = liftFunctionME (srcToResults @(Text,Text,Text))

csv4 :: ForeignFunc 5
csv4 = liftFunctionME (srcToResults @(Text,Text,Text,Text))

srcToResults :: forall a. CSV.FromRecord a => Text -> Foreign [ a ]
srcToResults filePath = do
  csvFileT <- lift $ readFile (T.unpack filePath)
  let csvFileBS = LT.encodeUtf8 $ fromStrict csvFileT
  let eCSV = CSV.decode CSV.NoHeader csvFileBS
  V.toList <$> withExceptT T.pack (except eCSV)