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

import Options.Applicative

data Stage =
    VanillaLex
  | VanillaParse
  | VanillaNormal
  | ExalogRangeRepair
  | ExalogWellMode
  | Exalog

stageParser :: Parser Stage
stageParser =
     stageFlag' VanillaLex        "lex"          "Lexer output"
 <|> stageFlag' VanillaParse      "parse"        "Parser output"
 <|> stageFlag' VanillaNormal     "normal"       "Normal form"
 <|> stageFlag' ExalogRangeRepair "range-repair" "Repair range restriction"
 <|> stageFlag' ExalogWellMode    "well-mode"    "Repair moding"

run :: RunOptions -> IO ()
run RunOptions{..} = do
  bs <- BS.fromStrict . encodeUtf8 <$> readFile file

  succeedOrDie (Stage.parse file) bs $ \ast ->
    succeedOrDie (Stage.wellModed file >=> uncurry S.solve) bs $ display ast

repl :: ReplOptions -> IO ()
repl opts = panic "REPL is not yet supported."

prettyPrint :: PPOptions Stage -> IO ()
prettyPrint PPOptions{..} = do
  bs <- BS.fromStrict . encodeUtf8 <$> readFile file
  case stage of
    VanillaLex        -> succeedOrDie (Stage.lex file) bs print
    VanillaParse      -> succeedOrDie (Stage.parse file) bs $ putStrLn . pp
    VanillaNormal     -> succeedOrDie (Stage.normalised file) bs $ putStrLn . pp
    ExalogRangeRepair -> succeedOrDie (Stage.rangeRestrictionRepaired file) bs printExalog
    ExalogWellMode    -> succeedOrDie (Stage.wellModed file) bs printExalog
  where
  printExalog (exalogProgram, initEDB) = do
    putStrLn $ pp exalogProgram
    putStrLn ("" :: Text)
    putStrLn $ pp initEDB

main :: IO ()
main = do
  command <- execParser (info (opts (ppOptions stageParser)) idm)
  case command of
    Run runOpts   -> run  runOpts
    Repl replOpts -> repl replOpts
    PrettyPrint ppOpts -> prettyPrint ppOpts
