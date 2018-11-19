module Language.Vanillalog.Generic.Transformation.Query (nameQueries) where

import Protolude

import qualified Data.ByteString.Lazy.Char8 as BS

import Language.Vanillalog.Generic.AST

nameQueries :: Program op -> Program op
nameQueries pr = evalState (sentential nameQuery pr) 0

nameQuery :: Sentence op -> State Int (Sentence op)
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

sentential :: Monad m
           => (Sentence op -> m (Sentence op)) -> Program op -> m (Program op)
sentential f (Program sentences) = Program <$> (traverse f $ sentences)
