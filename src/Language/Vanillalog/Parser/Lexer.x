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
import qualified Language.Vanillalog.Generic.Logger as Log
}

%wrapper "monadUserState-bytestring"

@idChar   = [a-zA-Z0-9_']
@var      = [A-Z]@idChar*
@fxSym    = [a-z]@idChar*

@int = [1-9][0-9]+

token :-

<0,scB> $white+  ;
<0,scB> "%".*    ;

<0,scB> "("      { basic TLeftPar }
<0,scB> ")"      { basic TRightPar }
<0,scB> ","      { basic TComma }
<scB>   ";"      { basic TSemicolon }
<scB>   "!"      { basic TNeg }

<0> ":-"         { basic TRule  `andBegin` scB }
<0> "?-"         { basic TQuery `andBegin` scB }
<0,scB> "."      { basic TDot   `andBegin` 0 }

<0,scB> @fxSym   { useInput TFxSym }
<0,scB> @var     { useInput TVariable }
<0,scB> @int     { useInput (TInt . read . BS.unpack) }
<0,scB> true     { basic (TBool True) }
<0,scB> false    { basic (TBool False) }

<0> \"           { begin str }
<str> [^\"]+     { useInput TStr }
<str> \"         { begin 0 }

<scB> \"         { begin strB }
<strB> [^\"]+    { useInput TStr }
<strB> \"        { begin scB }

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
  | TFxSym    { _str  :: str }
  | TVariable { _str  :: str }
  | TStr      { _str  :: str }
  | TInt      { _int  :: Int }
  | TBool     { _bool :: Bool }
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

lex :: FilePath -> BS.ByteString -> Log.LoggerM [ L.Lexeme (Token Text) ]
lex file source =
  case result of
    Right lexemes -> pure $ fmap (fmap (toStrict . decodeUtf8)) <$> lexemes
    Left msg      -> Log.scold Nothing (fromString msg)
  where
  result = runAlex source (setFile file >> lexM)

  lexM = do
    tok <- alexMonadScan
    if tok == eof
      then return [ eof ]
      else (tok :) <$> lexM
}
