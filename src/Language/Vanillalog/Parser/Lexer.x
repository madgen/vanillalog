{
{-# LANGUAGE DeriveFunctor #-}
module Language.Vanillalog.Parser.Lexer where

import Prelude
import Protolude (Text, bimap)

import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)

import qualified Data.ByteString.Lazy.Char8 as BS

import           Language.Vanillalog.Generic.Error (Error(..), Severity(..))
import           Language.Vanillalog.Generic.Parser.SrcLoc hiding (file)
import qualified Language.Vanillalog.Generic.Parser.Lexeme as L
}

%wrapper "monadUserState-bytestring"

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

<0> \"           { begin str }
<str> [^\"]+     { useInput TStr }
<str> \"         { begin 0 }

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
  | TID   { _str  :: str }
  | TStr  { _str  :: str }
  | TInt  { _int  :: Int }
  | TBool { _bool :: Bool }
  | TEOF
  deriving (Eq, Show, Functor)

basic :: Token str -> AlexAction (L.Lexeme (Token str))
basic = useInput . const

useInput :: (BS.ByteString -> Token str) -> AlexAction (L.Lexeme (Token str))
useInput f (aPos,_,inp,_) len = do
  file <- getFile
  return $ L.Lexeme (alexToSpan aPos file len) (f $ BS.take len inp)

-- Assumes all tokens are on the same line
alexToSpan :: AlexPosn -> FilePath -> Int64 -> SrcSpan
alexToSpan (AlexPn _ line col) file len =
  SrcSpan (SrcLoc file line col)
          (SrcLoc file line (col + (fromIntegral len) - 1))

eof :: L.Lexeme (Token str)
eof = L.Lexeme dummySpan TEOF

alexEOF :: Alex (L.Lexeme (Token str))
alexEOF = return eof

data AlexUserState = AlexUserState
  { file :: FilePath
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { file = "" }

setFile :: FilePath -> Alex ()
setFile file = Alex $ \s ->
  Right (s {alex_ust = (alex_ust s) {file = file}}, ())

getFile :: Alex FilePath
getFile = Alex $ \s -> Right (s, file . alex_ust $ s)

lex :: FilePath -> BS.ByteString -> Either [ Error ] [ L.Lexeme (Token Text) ]
lex file source =
  bimap (pure . Error User Nothing . fromString)
        (fmap (fmap (toStrict . decodeUtf8)) <$>) result
  where
  result = runAlex source (setFile file >> lexM)

  lexM = do
    tok <- alexMonadScan
    if tok == eof
      then return [ eof ]
      else (tok :) <$> lexM
}
