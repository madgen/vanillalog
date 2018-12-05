module Language.Vanillalog.Generic.Logger
  ( LoggerMT
  , LoggerM
  , runLoggerT
  , whisper
  , scold
  , scream
  , Error
  , Severity(..)
  ) where

import Protolude hiding (log)

import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import Language.Vanillalog.Generic.Pretty (pp)
import Language.Vanillalog.Generic.Error (Error(..), Severity(..), printError)
import Language.Vanillalog.Generic.Parser.SrcLoc (SrcSpan, printSpan)

type LoggerMT = MaybeT
type LoggerM  = LoggerMT IO

runLoggerT :: Monad m => LoggerMT m a -> m (Maybe a)
runLoggerT = runMaybeT

whisper :: MonadIO m => Maybe SrcSpan -> Text -> LoggerMT m ()
whisper mSpan msg =
  liftIO . printError $ Error Warning mSpan msg

scold :: MonadIO m => Maybe SrcSpan -> Text -> LoggerMT m a
scold mSpan msg = do
  liftIO . printError $ Error User mSpan msg
  MaybeT (return Nothing)

scream :: MonadIO m => Maybe SrcSpan -> Text -> LoggerMT m a
scream mSpan msg = do
  liftIO . printError $ Error Impossible mSpan msg
  MaybeT (return Nothing)