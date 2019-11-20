{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-name-shadowing #-}

module Language.Vanillalog.StageSpec (spec) where

import Protolude

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Csv as CSV
import qualified Data.Vector as V
import qualified Data.Text as T

import System.IO (openTempFile, hClose)
import System.Directory (removeFile)

import Test.Hspec

import qualified Language.Exalog.Core as E
import qualified Language.Exalog.KnowledgeBase.Set as KB
import qualified Language.Exalog.SrcLoc as Src

import qualified Language.Vanillalog.Stage as S

readSrc :: FilePath -> IO BS.ByteString
readSrc filePath = BS.fromStrict . encodeUtf8 <$> readFile filePath

runSrc :: FilePath -> IO (Maybe (KB.Set 'E.ABase))
runSrc srcPath = do
  bs <- readSrc srcPath
  let stageEnv = S.defaultStageEnv
        { S._input = S.Textual (Src.File srcPath) bs S.SProgram }
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

    describe "IO" $ do
      csvContents <- runIO $ do
        (outputPath, outputHandle) <- openTempFile "examples" "output.csv"
        hClose outputHandle

        let destinationPath = "examples/in/destination.csv"

        BS.writeFile destinationPath $ CSV.encode [ CSV.Only (T.pack outputPath) ]
        _ <- runSrc "examples/csv-io.vlog"
        removeFile destinationPath

        output <- BS.readFile outputPath
        removeFile outputPath

        pure $ CSV.decode @(Text,Text) CSV.NoHeader output

      it "writes CSV file" $
        csvContents `shouldBe` Right (V.fromList [ (i,j) | i <- ["1","2","3"], j <- ["1","2","3"] ])
