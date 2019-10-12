{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Protolude

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T

import qualified System.Console.Haskeline as HLine

import Options.Applicative hiding (command, header)

import           Language.Exalog.Pretty ()

import           Language.Vanillalog.Generic.Pretty (pp)
import qualified Language.Vanillalog.Stage as S

import Language.Vanillalog.Generic.CLI.Arguments
import Language.Vanillalog.Generic.CLI.Util

data Stage =
    VanillaLex
  | VanillaParse
  | VanillaNormal
  | Exalog
  | ExalogRangeRepair
  | ExalogWellMode
  | ExalogStratify

stageParser :: Parser Stage
stageParser =
     stageFlag' VanillaLex        "lex"          "Lexer output"
 <|> stageFlag' VanillaParse      "parse"        "Parser output"
 <|> stageFlag' VanillaNormal     "normal"       "Normal form"
 <|> stageFlag' Exalog            "exalog"       "Compile to Exalog"
 <|> stageFlag' ExalogRangeRepair "range-repair" "Repair range restriction"
 <|> stageFlag' ExalogWellMode    "well-mode"    "Repair moding"
 <|> stageFlag' ExalogStratify    "stratify"     "Stratify"

run :: RunOptions -> IO ()
run RunOptions{..} = do
  bs <- BS.fromStrict . encodeUtf8 <$> readFile _file

  let stageEnv =  S.StageEnv _file bs S.SProgram S.OnlyQueryPreds
  ast <- succeedOrDie stageEnv S.parse
  sol <- succeedOrDie stageEnv (S.solved mempty)
  display ast sol

repl :: ReplOptions -> IO ()
repl ReplOptions{..} = do
  putStrLn @Text "Interactive Vanillalog environment - REPL"

  baseSolution <- computeBase

  HLine.runInputT HLine.defaultSettings (loop baseSolution)
  where
  computeBase =  do
    bs <- BS.fromStrict . encodeUtf8 <$> readFile _file
    let stageEnv = S.StageEnv _file bs S.SProgram S.AllPreds
    succeedOrDie stageEnv (S.solved mempty)

  loop baseSol = do
    mInput <- HLine.getInputLine prefix
    case mInput of
      Nothing -> pure ()
      Just input
        | input `elem` [ ":e", ":exit", ":q", ":quit" ] -> pure ()
        | otherwise -> do -- Interpret it as a query
          mSolution <- lift $
            S.runStage (mkReplEnv $ prefix <> input) (S.solved baseSol)
          HLine.outputStrLn $ case mSolution of
            Just sol -> T.unpack $ pp sol
            Nothing  -> "Ill-formed query. Try again."
          loop baseSol

  prefix = "?- "
  mkReplEnv inp = S.StageEnv "STDIN" (BS.pack inp) S.SSentence S.OnlyQueryPreds

prettyPrint :: PPOptions Stage -> IO ()
prettyPrint PPOptions{..} = do
  bs <- BS.fromStrict . encodeUtf8 <$> readFile _file
  let stageEnv = S.StageEnv _file bs S.SProgram S.AllPreds
  case _stage of
    VanillaLex        -> print         =<< succeedOrDie stageEnv S.lex
    VanillaParse      -> putStrLn . pp =<< succeedOrDie stageEnv S.parse
    VanillaNormal     -> putStrLn . pp =<< succeedOrDie stageEnv S.normalised
    Exalog            -> printExalog   =<< succeedOrDie stageEnv S.compiled
    ExalogRangeRepair -> printExalog   =<< succeedOrDie stageEnv S.rangeRestrictionRepaired
    ExalogWellMode    -> printExalog   =<< succeedOrDie stageEnv S.wellModed
    ExalogStratify    -> printExalog   =<< succeedOrDie stageEnv S.stratified
  where
  printExalog (exalogProgram, initEDB) = do
    putStrLn $ pp exalogProgram
    putStrLn ("" :: Text)
    putStrLn $ pp initEDB

main :: IO ()
main = do
  command <-
    customExecParser
      (prefs showHelpOnEmpty)
      (optsParser (fromStageParser stageParser) header)
  case command of
    Run runOpts        -> run runOpts
    Repl replOpts      -> repl replOpts
    PrettyPrint ppOpts -> prettyPrint ppOpts
  where
  header = "vanillalog - a simple Datalog compiler"
