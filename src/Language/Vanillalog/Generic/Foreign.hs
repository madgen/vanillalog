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
  , read_csv1
  , read_csv2
  , read_csv3
  , read_csv4
  , append_csv1
  , append_csv2
  , append_csv3
  , append_csv4
  ) where

import Protolude hiding (subtract)

import Control.Monad.Trans.Except (except, withExceptT)

import qualified Data.Csv as CSV
import qualified Data.Text as T
import qualified Data.Text.IO as T
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

read_csv1 :: ForeignFunc 2
read_csv1 = liftFunctionME (fmap (CSV.fromOnly <$>) <$> srcToResults @(CSV.Only Text))

read_csv2 :: ForeignFunc 3
read_csv2 = liftFunctionME (srcToResults @(Text,Text))

read_csv3 :: ForeignFunc 4
read_csv3 = liftFunctionME (srcToResults @(Text,Text,Text))

read_csv4 :: ForeignFunc 5
read_csv4 = liftFunctionME (srcToResults @(Text,Text,Text,Text))

srcToResults :: forall a. CSV.FromRecord a => Text -> Foreign [ a ]
srcToResults filePath = do
  csvFileT <- lift $ readFile (T.unpack filePath)
  let csvFileBS = LT.encodeUtf8 $ fromStrict csvFileT
  let eCSV = CSV.decode CSV.NoHeader csvFileBS
  V.toList <$> withExceptT T.pack (except eCSV)

append_csv1 :: ForeignFunc 2
append_csv1 = liftPredicateME go
  where
  go :: Text -> Text -> Foreign Bool
  go src field = appendToSrc src (CSV.Only field)

append_csv2 :: ForeignFunc 3
append_csv2 = liftPredicateME go
  where
  go :: Text -> Text -> Text -> Foreign Bool
  go src f1 f2 = appendToSrc src (f1,f2)

append_csv3 :: ForeignFunc 4
append_csv3 = liftPredicateME go
  where
  go :: Text -> Text -> Text -> Text -> Foreign Bool
  go src f1 f2 f3 = appendToSrc src (f1,f2,f3)

append_csv4 :: ForeignFunc 5
append_csv4 = liftPredicateME go
  where
  go :: Text -> Text -> Text -> Text -> Text -> Foreign Bool
  go src f1 f2 f3 f4 = appendToSrc src (f1,f2,f3,f4)

appendToSrc :: CSV.ToRecord f => Text -> f -> Foreign Bool
appendToSrc src record = do
  let toAppend = toStrict $ LT.decodeUtf8 $ CSV.encode [ record ]
  lift $ T.appendFile (T.unpack src) toAppend
  pure True
