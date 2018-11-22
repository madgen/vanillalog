module Language.Vanillalog.Generic.Logger
  ( LoggerMT
  , LoggerM
  , runLoggerT
  , runLogger
  , log
  , scream
  , Error
  , Severity(..)
  ) where

import Protolude hiding (log)

import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)

import Language.Vanillalog.Generic.Error (Error(..), Severity(..))
import Language.Vanillalog.Generic.Parser.SrcLoc (SrcSpan)

type LoggerMT = WriterT [ Error ]
type LoggerM = LoggerMT Identity

runLoggerT :: Monad m => LoggerMT m a -> m (Either [ Error ] a)
runLoggerT action = do
  (ma,msgs) <- runWriterT action
  pure $ if null msgs then Right ma else Left msgs

runLogger :: LoggerM a -> Either [ Error ] a
runLogger = runIdentity . runLoggerT

log :: Monad m => Severity -> Maybe SrcSpan -> Text -> LoggerMT m ()
log severity mSpan = tell . pure . Error severity mSpan

scream :: Monad m => Severity -> Maybe SrcSpan -> Text -> LoggerMT m a
scream severity mSpan msg = log severity mSpan msg >> panic msg
