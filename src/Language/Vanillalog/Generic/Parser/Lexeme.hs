{-# LANGUAGE DeriveFunctor #-}

module Language.Vanillalog.Generic.Parser.Lexeme where

import Protolude

import Language.Exalog.SrcLoc (SrcSpan)

data Lexeme tok = Lexeme
  { _span :: SrcSpan
  , _token :: tok
  } deriving (Eq, Show, Functor)
