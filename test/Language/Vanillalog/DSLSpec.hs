{-# LANGUAGE DataKinds #-}

module Language.Vanillalog.DSLSpec where

import Protolude

import qualified Data.Vector.Sized as V
import           Data.Maybe (fromJust)
import           Data.Singletons (sing)

import Test.Hspec

import qualified Language.Exalog.Core as E
import qualified Language.Exalog.KnowledgeBase.Class as KB
import qualified Language.Exalog.KnowledgeBase.Knowledge as KB
import qualified Language.Exalog.KnowledgeBase.Set as KB
import qualified Language.Exalog.SrcLoc as Src

import Language.Vanillalog.DSL

ancestorPr :: Vanillalog
ancestorPr = do
  let adviser  = predicate "adviser"
  let ancestor = predicate "ancestor"
  let (x,y,t) = (var "X", var "Y", var "T")
  let descendant = var "Descendant"

  Fact|> adviser("Alonzo Church","Alan Turing")
  Fact|> adviser("Alonzo Church","Raymond Smullyan")
  Fact|> adviser("Alonzo Church","Stephen Kleene")
  Fact|> adviser("Oswald Veblen","Alonzo Church")

  ancestor(x,y) -| adviser(x,y)
  ancestor(x,y) -| adviser(x,t) /\ ancestor(t,y)

  Query|> ancestor("Oswald Veblen",descendant)

expectedAncestors :: KB.Set 'E.ABase
expectedAncestors = KB.fromList
                  $ KB.Knowledge E.KnowABase ancestorPred
                <$> tuples
  where
  ancestorPred :: E.Predicate 1 'E.ABase
  ancestorPred = E.Predicate (E.PredABase Src.NoSpan) "query_0" sing E.Logical

  tuples :: [ V.Vector 1 E.Sym ]
  tuples = fmap E.SymText . fromJust . V.fromList <$>
    [ [ "Alan Turing" ], [ "Raymond Smullyan" ], [ "Stephen Kleene" ], [ "Alonzo Church" ] ]

spec :: Spec
spec =
  describe "DSL" $
    it "runs ancestor program" $ do
      output <- runDatalog mempty ancestorPr
      output `shouldBe` Just expectedAncestors
