module Language.Vanillalog.Generic.Stage
  ( Stage
  , StageEnv(..)
  , ParserScope(..)
  , runStageT
  ) where

import Protolude

import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Language.Exalog.Logger as Log

data ParserScope = SProgram | SSentence

data StageEnv = StageEnv
  { _file        :: FilePath
  , _input       :: BS.ByteString
  , _parserScope :: ParserScope
  }

runStageT :: StageEnv -> Stage a -> Log.Logger a
runStageT env = (`runReaderT` env)

type Stage a = ReaderT StageEnv Log.Logger a
