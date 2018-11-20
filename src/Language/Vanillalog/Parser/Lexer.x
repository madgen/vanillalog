{
{-# LANGUAGE DeriveFunctor #-}
module Language.Vanillalog.Parser.Lexer where

import Prelude
import Protolude (Text, bimap)

import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)

import qualified Data.ByteString.Lazy.Char8 as BS
}

%wrapper "monad-bytestring"

token :-

<0> $white+      ;
<0> "%".*\n      ;

<0> "("          { basic TLeftPar }
<0> ")"          { basic TRightPar }
<0> "."          { basic TDot }
<0> ","          { basic TComma }
<0> ";"          { basic TSemicolon }
<0> ":-"         { basic TRule }
<0> "?-"         { basic TQuery }
<0> "!"          { basic TNeg }

<0> [a-zA-Z_]+   { useInput TID  }
<0> [1-9][0-9]*  { useInput (TInt . read . BS.unpack) }
<0> true         { basic (TBool True) }
<0> false        { basic (TBool False) }

<0> \"           { begin str}
<str> [^\"]+     { useInput TStr }
<str> \"         { begin 0}

{
data Token str =
    TLeftPar
  | TRightPar
  | TDot
  | TComma
  | TSemicolon
  | TRule
  | TQuery
  | TNeg
  | TID   str
  | TStr  str
  | TInt  Int
  | TBool Bool
  | TEOF
  deriving (Eq, Show, Functor)

basic :: Token str -> AlexAction (Token str)
basic tok = token (\_ _ -> tok)

useInput :: (BS.ByteString -> (Token str)) -> AlexAction (Token str)
useInput f = token (\(_,_,inp,_) len -> f (BS.take len inp))

alexEOF :: Alex (Token str)
alexEOF = return TEOF

lex :: BS.ByteString -> Either [ Text ] [ Token Text ]
lex text =
  bimap (pure . fromString) (fmap (toStrict . decodeUtf8) <$>) $ runAlex text go
  where
  go = do
    tok <- alexMonadScan
    if tok == TEOF
      then return [ TEOF ]
      else (tok :) <$> go
}
