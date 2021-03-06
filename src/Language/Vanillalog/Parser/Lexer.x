{
{-# LANGUAGE DeriveFunctor #-}
module Language.Vanillalog.Parser.Lexer where

import Prelude
import Protolude (Text, toStrict)

import           Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Language.Exalog.Logger as Log
import           Language.Exalog.SrcLoc

import qualified Language.Vanillalog.Generic.Parser.Lexeme as L

#ifdef DEBUG
import Debug.Trace
#endif
}

%wrapper "monadUserState-bytestring"

@idChar   = [a-zA-Z0-9_']
@var      = [A-Z]@idChar*
@wild     = _@var?
@fxSym    = [a-z]@idChar*

@positive = [1-9][0-9]*
@int      = 0|@positive|\-@positive

token :-

<0,scB,scA> $white+  ;
<0>         "%".*    ;

<0,scB> "("  { basic TLeftPar }
<0,scB> ")"  { basic TRightPar }
<scA>   ","  { basic TComma }

<scB> ","        { basic TConj }
<scB> ";"        { basic TDisj }
<scB> "!"        { basic TNeg }

<0>   ":-"     { enterStartCodeAnd scB $ basic TRule }
<0>   "?-"     { enterStartCodeAnd scB $ basic TQuery }
<scB> "."      { exitStartCodeAnd $ basic TDot }
<0>   "."      { basic TDot }

<0,scB> @fxSym   { enterStartCodeAnd scA $ useInput TFxSym }
<scA>   "("      { basic TLeftPar }
<scA>   ")"      { exitStartCodeAnd $ basic TRightPar }
<scA>   true     { basic (TBool True) }
<scA>   false    { basic (TBool False) }
<scA>   @var     { useInput TVariable }
<scA>   @wild    { basic TWildcard }
<scA>   @int     { useInput (TInt . read . BS.unpack) }

<scA> \"\"       { basic (TStr "") } -- Empty string
<scA> \"         { enterStartCodeAnd str skip }
<str> [^\"]*     { useInput TStr }
<str> \"         { exitStartCodeAnd skip }

{
data Token str =
    TLeftPar
  | TRightPar
  | TDot
  | TComma
  | TRule
  | TQuery
  | TConj
  | TDisj
  | TNeg
  | TFxSym    { _tStr  :: str }
  | TVariable { _tStr  :: str }
  | TWildcard
  | TStr      { _tStr  :: str }
  | TInt      { _tInt  :: Int }
  | TBool     { _tBool :: Bool }
  | TEOF
  deriving (Eq, Show, Functor)

basic :: Token str -> AlexAction (L.Lexeme (Token str))
basic = useInput . const

useInput :: (BS.ByteString -> Token str) -> AlexAction (L.Lexeme (Token str))
useInput f (aPos,_,inp,_) len = do
  inpSrc <- getInputSource
  return $ L.Lexeme (alexToSpan aPos inpSrc len) (f $ BS.take len inp)

-- Assumes all tokens are on the same line
alexToSpan :: AlexPosn -> InputSource -> Int64 -> SrcSpan
alexToSpan (AlexPn _ line col) inpSrc len =
  Span inpSrc (SrcLoc line col)
              (SrcLoc line (col + (fromIntegral len) - 1))

eof :: L.Lexeme (Token str)
eof = L.Lexeme NoSpan TEOF

alexEOF :: Alex (L.Lexeme (Token str))
alexEOF = return eof

data AlexUserState = AlexUserState
  { _inputSource :: InputSource
  , _startCodeStack :: [ Int ]
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {_inputSource = None, _startCodeStack = []}

getUserState :: Alex AlexUserState
getUserState = Alex $ \s -> Right (s, alex_ust $ s)

modifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUserState f =
  Alex $ \s -> Right (s {alex_ust = f (alex_ust s)}, ())

setUserState :: AlexUserState -> Alex ()
setUserState = modifyUserState . const

getInputSource :: Alex InputSource
getInputSource = _inputSource <$> getUserState

setInputSource :: InputSource -> Alex ()
setInputSource inpSrc = modifyUserState (\s -> s {_inputSource = inpSrc})

pushStartCode :: Int -> Alex ()
pushStartCode startCode =
  modifyUserState (\s -> s {_startCodeStack = startCode : _startCodeStack s})

topStartCode :: Alex Int
topStartCode = do
  stack <- _startCodeStack <$> getUserState
  case stack of
    (x:_) -> return x
    _     -> Alex . const $
      Left "Impossible: The lexer start code stack is empty."

popStartCode :: Alex Int
popStartCode = do
  startCode <- topStartCode
  modifyUserState (\s -> s {_startCodeStack = tail . _startCodeStack $ s})
  pure startCode

enterStartCode' :: Int -> Alex ()
enterStartCode' newStartCode = do
  currentStartCode <- alexGetStartCode
  pushStartCode currentStartCode
  alexSetStartCode newStartCode

exitStartCode' :: Alex ()
exitStartCode' = do
  startCodeToReturn <- popStartCode
  alexSetStartCode startCodeToReturn

enterStartCodeAnd :: Int -> AlexAction a -> AlexAction a
enterStartCodeAnd startCode action inp len =
  enterStartCode' startCode *> action inp len

exitStartCodeAnd :: AlexAction a -> AlexAction a
exitStartCodeAnd action inp len = exitStartCode' *> action inp len

lex :: InputSource -> BS.ByteString -> Log.Logger [ L.Lexeme (Token Text) ]
lex inpSrc source =
  case result of
    Right lexemes -> pure $ fmap (fmap (toStrict . decodeUtf8)) <$> lexemes
    Left msg      -> Log.scold NoSpan (fromString msg)
  where
  result = runAlex source (setInputSource inpSrc >> lexM)

  lexM = do
    tok <- alexMonadScan

#if defined (DEBUG) && defined (LEXER)
    traceShowM tok
    stack <- fmap StartCode . _startCodeStack <$> getUserState
    traceM $ "Start code stack: " <> show stack
    startCode <- alexGetStartCode
    traceM $ "Current start code: " <> show (StartCode startCode)
#endif

    if tok == eof
      then return [ eof ]
      else (tok :) <$> lexM

#if defined (DEBUG) && defined (LEXER)
newtype StartCode = StartCode Int

instance Show StartCode where
  show (StartCode i) =
    if i == 0         then "0"
    else if i == scA  then "atom"
    else if i == scB  then "body"
    else if i == str  then "string"
    else error "Unknown start code"
#endif
}
