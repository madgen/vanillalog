{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Vanillalog.Generic.Parser.SrcLoc
  ( SrcLoc(..)
  , SrcSpan(..)
  , dummySpan
  , transSpan
  , listSpan
  , Spannable(..)
  ) where

import Protolude hiding ((<>), empty, SrcLoc)

import Data.Maybe (fromJust)

import Text.PrettyPrint

import Language.Exalog.Pretty.Helper

data SrcLoc =
    SrcLoc
      { file :: !FilePath
      , line :: !Int
      , col  :: !Int
      }
  | SrcDummy
  deriving (Eq, Show)

data SrcSpan = SrcSpan SrcLoc SrcLoc deriving (Eq, Show)

dummySpan :: SrcSpan
dummySpan = SrcSpan SrcDummy SrcDummy

isBefore :: SrcLoc -> SrcLoc -> Bool
isBefore loc@SrcLoc{} loc'@SrcLoc{} =
  file loc == file loc' && -- In the same file
  ( line loc < line loc' || -- line number of loc precedes loc'
    ( line loc == line loc' &&
      col loc < col loc')) -- or on the same line but loc is at a preceding col.
isBefore _ _ = False

transSpan :: SrcSpan -> SrcSpan -> Maybe SrcSpan
transSpan (SrcSpan loc1 loc2) (SrcSpan loc2' loc3) =
  if loc2 `isBefore` loc2' then Just (SrcSpan loc1 loc3) else Nothing

listSpan :: [ SrcSpan ] -> Maybe SrcSpan
listSpan []             = Nothing
listSpan [ span ]       = Just span
listSpan (span : spans) = transSpan span =<< listSpan spans

--------------------------------------------------------------------------------
-- Spans of various nodes
--------------------------------------------------------------------------------

class Spannable a where
  span :: a -> SrcSpan

instance {-# OVERLAPPABLE #-} HasField "_span" r SrcSpan => Spannable r where
  span = getField @"_span"

-- |Unsafe
instance {-# OVERLAPPING #-} (Spannable a, Spannable b) => Spannable (a,b) where
  span (a,b) = fromJust $ transSpan (span a) (span b)

-- |Unsafe
instance {-# OVERLAPPING #-} Spannable a => Spannable [ a ] where
  span as = fromJust $ listSpan (map span as)

--------------------------------------------------------------------------------
-- Pretty instances
--------------------------------------------------------------------------------

instance Pretty SrcLoc where
  pretty SrcLoc{..} = int line <> colon <> int col <> " in " <> text file
  pretty SrcDummy   = empty

-- |This is really ought to be better.
instance Pretty SrcSpan where
  pretty (SrcSpan loc1 loc2) =
    "From " <> pretty loc1 $+$ nest 2 ("to " <?> pretty loc2 <> ".")

instance Pretty (Maybe SrcSpan) where
  pretty Nothing     = empty
  pretty (Just span) = pretty span
