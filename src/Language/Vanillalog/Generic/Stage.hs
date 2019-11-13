{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Vanillalog.Generic.Stage
  ( Stage
  , StageEnv(..)
  , Input(..)
  , ParserScope(..)
  , KeepPredicates(..)
  , defaultStageEnv
  , runStage
  ) where

import Protolude hiding (decodeUtf8)

import           Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as BS

import           Language.Exalog.SrcLoc (InputSource(..))
import qualified Language.Exalog.Logger as Log

import qualified Language.Vanillalog.Generic.AST as AG

data ParserScope = SProgram | SSentence

data KeepPredicates = OnlyQueryPreds | AllPreds

type ReservedNames = [ Text ]

data Input decl hop bop =
    Textual
    { _inputSource :: InputSource
    , _source      :: BS.ByteString
    , _parserScope :: ParserScope
    }
  | AST
    { _ast :: AG.Program decl hop bop
    }

data StageEnv decl hop bop = StageEnv
  { _input          :: Input decl hop bop
  , _keepPredicates :: KeepPredicates
  , _reservedNames  :: ReservedNames
  }

defaultStageEnv :: StageEnv decl hop bop
defaultStageEnv = StageEnv (Textual None "" SProgram) OnlyQueryPreds []

runStage :: StageEnv decl hop bop -> Stage decl hop bop a -> IO (Maybe a)
runStage env@StageEnv{..} = Log.runLoggerT loggerEnv . (`runReaderT` env)
  where
  loggerEnv = Log.LoggerEnv $
    case _input of
      Textual{_source} -> Just (toStrict . decodeUtf8 $ _source)
      AST{}            -> Nothing

type Stage decl hop bop a = ReaderT (StageEnv decl hop bop) Log.Logger a
