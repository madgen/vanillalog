module Language.Vanillalog.Query (nameQueries) where

import Protolude

import qualified Data.ByteString.Lazy.Char8 as BS

import Language.Vanillalog.AST

nameQueries :: Program -> Program
nameQueries pr = evalState (sentential nameQuery pr) 0

nameQuery :: Sentence -> State Int Sentence
nameQuery (SQuery (Query Nothing body)) =
  SQuery <$> (Query <$> (Just <$> ac) <*> pure body)
  where
  ac :: State Int AtomicFormula
  ac = do
    name <- freshQueryName
    pure $ AtomicFormula name $ TVar <$> vars body

  freshQueryName :: State Int BS.ByteString
  freshQueryName = do
    modify (+ 1)
    BS.pack . ("query_" <>) . show <$> get
nameQuery SQuery{} = panic "Impossible: Query has already been named."
nameQuery s = return s

sentential :: Monad m => (Sentence -> m Sentence) -> Program -> m Program
sentential f (Program sentences) = Program <$> (traverse f $ sentences)
