module Language.Vanillalog.Generic.Logger where

import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)

import Protolude hiding (log)

type LoggerMT = WriterT [ Text ]
type LoggerM = LoggerMT Identity

runLoggerT :: Monad m => LoggerMT m a -> m (Either [ Text ] a)
runLoggerT action = do
  (ma,msgs) <- runWriterT action
  pure $ if null msgs then Right ma else Left msgs

runLogger :: LoggerM a -> Either [ Text ] a
runLogger = runIdentity . runLoggerT

log :: Monad m => Text -> LoggerMT m ()
log = tell . pure

scream :: Monad m => Text -> LoggerMT m a
scream msg = log msg >> panic msg
