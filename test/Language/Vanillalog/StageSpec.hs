{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-name-shadowing #-}

module Language.Vanillalog.StageSpec (spec) where

import Protolude

import qualified Data.ByteString.Lazy.Char8 as BS

import Test.Hspec

import qualified Language.Exalog.Core as E
import qualified Language.Exalog.Relation as R
import qualified Language.Exalog.SrcLoc as Src

import qualified Language.Vanillalog.Stage as S

readSrc :: FilePath -> IO BS.ByteString
readSrc filePath = BS.fromStrict . encodeUtf8 <$> readFile filePath

runSrc :: FilePath -> IO (Maybe (R.Solution 'E.ABase))
runSrc srcPath = do
  bs <- readSrc srcPath
  let stageEnv = S.defaultStageEnv { S._inputSource = Src.File srcPath, S._input = bs }
  S.runStage stageEnv (S.solved mempty)

testSolving :: FilePath -> SpecWith ()
testSolving srcPath = do
  mSolution <- runIO $ runSrc srcPath

  it "executes" $ mSolution `shouldSatisfy` isJust
  it "solves" $ pendingWith "Exalog to make solution fixtures available"

spec :: Spec
spec =
  describe "Integration tests for compilation stages" $ do
    describe "linear ancestor" $ do
      testSolving "examples/linear-ancestor.vlog"

      mSol  <- runIO $ runSrc "examples/linear-ancestor.vlog"
      mSol' <- runIO $ runSrc "examples/linear-ancestor-csv-io.vlog"
      it "produces the same result for IO and pure versions" $
        mSol' `shouldBe` mSol

    describe "non-linear ancestor" $
      testSolving "examples/non-linear-ancestor.vlog"

    describe "complement graph" $
      testSolving "examples/complement-graph.vlog"
