module Language.Vanillalog.Generic.Stage
  ( Stage
  , StageEnv(..)
  , ParserScope(..)
  , KeepPredicates(..)
  , defaultStageEnv
  , runStage
  ) where

import Protolude

import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Language.Exalog.Logger as Log

data ParserScope = SProgram | SSentence

data KeepPredicates = OnlyQueryPreds | AllPreds

type ReservedNames = [ Text ]

data StageEnv = StageEnv
  { _file           :: FilePath
  , _input          :: BS.ByteString
  , _parserScope    :: ParserScope
  , _keepPredicates :: KeepPredicates
  , _reservedNames  :: ReservedNames
  }

defaultStageEnv :: StageEnv
defaultStageEnv = StageEnv "STDIN" "" SProgram OnlyQueryPreds []

runStage :: StageEnv -> Stage a -> IO (Maybe a)
runStage env = Log.runLoggerT . (`runReaderT` env)

type Stage a = ReaderT StageEnv Log.Logger a
