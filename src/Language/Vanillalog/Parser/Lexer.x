{
module Language.Vanillalog.Parser.Lexer where

import Prelude

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
<0> "!"          { basic TNeg }

<0> [a-zA-Z_]+   { useInput TID  }
<0> [1-9][0-9]*  { useInput (TInt . read . BS.unpack) }
<0> true         { basic (TBool True) }
<0> false        { basic (TBool False) }

<0> \"           { begin str}
<str> [^\"]+     { useInput TStr }
<str> \"         { begin 0}

{
data Token =
    TLeftPar
  | TRightPar
  | TDot
  | TComma
  | TSemicolon
  | TRule
  | TNeg
  | TID   BS.ByteString
  | TStr  BS.ByteString
  | TInt  Int
  | TBool Bool
  | TEOF
  deriving (Eq, Show)

basic :: Token -> AlexAction Token
basic tok = token (\_ _ -> tok)

useInput :: (BS.ByteString -> Token) -> AlexAction Token
useInput f = token (\(_,_,inp,_) len -> f (BS.take len inp))

alexEOF :: Alex Token
alexEOF = return TEOF

lex :: BS.ByteString -> Either String [ Token ]
lex text = runAlex text go
  where
  go = do
    tok <- alexMonadScan
    if tok == TEOF
      then return [ TEOF ]
      else (tok :) <$> go
}
