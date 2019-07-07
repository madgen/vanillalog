{-# LANGUAGE RecordWildCards #-}

module Main where

import Protolude

import qualified Data.ByteString.Lazy.Char8 as BS

import           Language.Exalog.Pretty ()
import qualified Language.Exalog.Solver as S

import           Language.Vanillalog.Generic.Pretty (pp)
import qualified Language.Vanillalog.Stage as Stage

import Language.Vanillalog.Generic.CLI.Arguments
import Language.Vanillalog.Generic.CLI.Util

import Options.Applicative hiding (command)

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

  succeedOrDie (Stage.parse _file) bs $ \ast ->
    succeedOrDie (Stage.stratified _file >=> uncurry S.solve) bs $ display ast

repl :: ReplOptions -> IO ()
repl _ = panic "REPL is not yet supported."

prettyPrint :: PPOptions Stage -> IO ()
prettyPrint PPOptions{..} = do
  bs <- BS.fromStrict . encodeUtf8 <$> readFile _file
  case _stage of
    VanillaLex        -> succeedOrDie (Stage.lex _file) bs print
    VanillaParse      -> succeedOrDie (Stage.parse _file) bs $ putStrLn . pp
    VanillaNormal     -> succeedOrDie (Stage.normalised _file) bs $ putStrLn . pp
    Exalog            -> succeedOrDie (Stage.compiled _file) bs printExalog
    ExalogRangeRepair -> succeedOrDie (Stage.rangeRestrictionRepaired _file) bs printExalog
    ExalogWellMode    -> succeedOrDie (Stage.wellModed _file) bs printExalog
    ExalogStratify    -> succeedOrDie (Stage.stratified _file) bs printExalog
  where
  printExalog (exalogProgram, initEDB) = do
    putStrLn $ pp exalogProgram
    putStrLn ("" :: Text)
    putStrLn $ pp initEDB

main :: IO ()
main = do
  command <- execParser (info (opts (fromStageParser stageParser)) idm)
  case command of
    Run runOpts   -> run  runOpts
    Repl replOpts -> repl replOpts
    PrettyPrint ppOpts -> prettyPrint ppOpts
