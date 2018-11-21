{-# LANGUAGE DeriveFunctor #-}

module Language.Vanillalog.Generic.Parser.Lexeme where

import Protolude

import Language.Vanillalog.Generic.Parser.SrcLoc (SrcSpan, Spannable(..))

data Lexeme tok = Lexeme
  { _span :: SrcSpan
  , token :: tok
  } deriving (Eq, Show, Functor)

instance Spannable (Lexeme tok) where
  span = _span
