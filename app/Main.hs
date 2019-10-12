{-# LANGUAGE RecordWildCards #-}

module Main where

import Protolude

import qualified Data.ByteString.Lazy.Char8 as BS

import           Language.Exalog.Pretty ()
import qualified Language.Exalog.Solver as Solver

import           Language.Vanillalog.Generic.Pretty (pp)
import qualified Language.Vanillalog.Stage as S

import Language.Vanillalog.Generic.CLI.Arguments
import Language.Vanillalog.Generic.CLI.Util

import Options.Applicative hiding (command, header)

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

  let stageEnv =  S.StageEnv _file bs S.SProgram S.AllPreds
  ast <- succeedOrDie stageEnv S.parse
  sol <- succeedOrDie stageEnv S.solved
  display ast sol

repl :: ReplOptions -> IO ()
repl _ = do
  putStrLn @Text "Interactive Vanillalog environment - REPL"
  HLine.runInputT HLine.defaultSettings loop
  where
  loop :: HLine.InputT IO ()
  loop = do
    minput <- HLine.getInputLine "?- "
    case minput of
      Nothing      -> pure ()
      Just command
        | command `elem` [ ":e", ":exit",  ":q", ":quit" ] -> pure ()
        | otherwise -> do -- Interpret it as a query
          -- sentenceParser ("?- " ++ command)
          return ()
--      Just input -> do
--        HLine.outputStrLn $ "Input was: " ++ input
--        loop

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
    Run runOpts   -> run  runOpts
    Repl replOpts -> repl replOpts
    PrettyPrint ppOpts -> prettyPrint ppOpts
  where
  header = "vanillalog - a simple Datalog compiler"
