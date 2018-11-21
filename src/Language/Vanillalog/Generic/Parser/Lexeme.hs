{-# LANGUAGE DeriveFunctor #-}

module Language.Vanillalog.Generic.Parser.Lexeme where

import Protolude

import Language.Vanillalog.Generic.Parser.SrcLoc (SrcSpan)

data Lexeme tok = Lexeme
  { span :: SrcSpan
  , token :: tok
  } deriving (Eq, Show, Functor)
