{-# LANGUAGE RecordWildCards #-}

module Main where

import Protolude

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T

import           Language.Exalog.Pretty ()
import qualified Language.Exalog.Solver as S

import Language.Vanillalog.AST (Program)
import Language.Vanillalog.Generic.Compiler (compile)
import Language.Vanillalog.Generic.Pretty (pp)
import Language.Vanillalog.Generic.Transformation.Query (nameQueries)
import Language.Vanillalog.Transformation.Normaliser (normalise)
import Language.Vanillalog.Parser.Lexer (lex)
import Language.Vanillalog.Parser.Parser (programParser)

import Language.Vanillalog.Generic.CLI.Arguments
import Language.Vanillalog.Generic.CLI.Util

import Options.Applicative

data Stage = VanillaLex | VanillaParse | VanillaNormal | Exalog

stageParser :: Parser Stage
stageParser =
   ( stageFlag' VanillaLex    "vanilla-lex"    "Tokenize"
 <|> stageFlag' VanillaParse  "vanilla-parse"  "Parse"
 <|> stageFlag' VanillaNormal "vanilla-normal" "Transform to normal form"
 <|> stageFlag' Exalog        "exalog"         "Compile to Exalog"
   )

run :: RunOptions -> IO ()
run RunOptions{..} = do
  bs <- BS.fromStrict . encodeUtf8 <$> readFile file
  succeedOrDie programParser bs $ \ast -> do
    case compile <$> (nameQueries >=> normalise) ast of
      Right (exalogProgram, initEDB) -> do
        finalEDB <- S.solve exalogProgram initEDB
        putStrLn $ pp finalEDB
      Left errs -> panic $ T.intercalate "\n" errs

repl :: ReplOptions -> IO ()
repl opts = panic "REPL is not yet supported."

prettyPrint :: PPOptions Stage -> IO ()
prettyPrint PPOptions{..} = do
  bs <- BS.fromStrict . encodeUtf8 <$> readFile file
  case stage of
    VanillaLex -> succeedOrDie lex bs print
    VanillaParse -> succeedOrDie programParser bs $ putStrLn . pp
    VanillaNormal ->
      succeedOrDie programParser bs $ \ast ->
        case nameQueries >=> normalise $ ast of
          Right ast' -> putStrLn . pp $ ast'
          Left errs -> panic $ T.intercalate "\n" errs
    Exalog ->
      succeedOrDie programParser bs $ \ast ->
        case compile <$> (nameQueries >=> normalise) ast of
          Right (exalogProgram, initEDB) -> do
            putStrLn $ pp exalogProgram
            putStrLn ("" :: Text)
            putStrLn $ pp initEDB
          Left errs -> panic $ T.intercalate "\n" errs

main :: IO ()
main = do
  command <- execParser (info (opts (ppOptions stageParser)) idm)
  case command of
    Run runOpts   -> run  runOpts
    Repl replOpts -> repl replOpts
    PrettyPrint ppOpts -> prettyPrint ppOpts
