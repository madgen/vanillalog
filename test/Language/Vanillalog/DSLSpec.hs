{-# LANGUAGE DataKinds #-}

module Language.Vanillalog.DSLSpec where

import qualified Protolude as P
import           Protolude hiding ((.),(-))

import qualified Data.Vector.Sized as V
import           Data.Maybe (fromJust)
import           Data.Singletons (sing)

import Test.Hspec

import qualified Language.Exalog.Core as E
import qualified Language.Exalog.Relation as R
import qualified Language.Exalog.SrcLoc as Src
import qualified Language.Exalog.Tuples as T

import Language.Vanillalog.DSL

ancestorPr :: Datalog
ancestorPr =
  let adviser  = mkPredicate2 "adviser"
      ancestor = mkPredicate2 "ancestor"
      (x,y,t) = (var "X", var "Y", var "T")
      descendant = var "Descendant"
  in Fact|> adviser("Alonzo Church","Alan Turing").
     Fact|> adviser("Alonzo Church","Raymond Smullyan").
     Fact|> adviser("Alonzo Church","Stephen Kleene").
     Fact|> adviser("Oswald Veblen","Alonzo Church").

     ancestor(x,y) |- adviser(x,y).
     ancestor(x,y) |- adviser(x,t) /\ ancestor(t,y).

     Query|> ancestor("Oswald Veblen",descendant).
     voila

expectedAncestors :: R.Solution 'E.ABase
expectedAncestors = R.fromList
                P.. (:[])
                P.. R.Relation ancestorPred
                P.. T.fromList
                $ tuples
  where
  ancestorPred :: E.Predicate 1 'E.ABase
  ancestorPred = E.Predicate (E.PredABase Src.NoSpan) "ancestor" sing E.Logical

  tuples :: [ V.Vector 1 E.Sym ]
  tuples = fmap E.SymText
       P.. fromJust
       P.. V.fromList <$>
    [ [ "Alan Turing" ], [ "Raymond Smullyan" ], [ "Stephen Kleene" ], [ "Alonzo Church" ] ]

spec :: Spec
spec =
  describe "DSL" $ do
    it "runs ancestor program" $ do
      output <- runDatalog ancestorPr
      output `shouldBe` Just expectedAncestors
