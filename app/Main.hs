{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Protolude

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.String (fromString)

import           Language.Exalog.Pretty ()
import qualified Language.Exalog.Solver as S

import Language.Vanillalog.AST (Program)
import Language.Vanillalog.Generic.Compiler (compile)
import Language.Vanillalog.Generic.Pretty (pp)
import Language.Vanillalog.Generic.Query (nameQueries)
import Language.Vanillalog.Normaliser (normalise)
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

-- Functionality to run

run :: RunOptions-> IO ()
run RunOptions{..} = do
  bs <- BS.fromStrict . encodeUtf8 <$> readFile file
  parseOrDie programParser bs $ \ast -> do
    let (exalogProgram, initEDB) = compile
                                 . normalise
                                 . nameQueries
                                 $ ast
    finalEDB <- S.solve exalogProgram initEDB
    putStrLn $ pp finalEDB

repl :: ReplOptions -> IO ()
repl opts = panic "REPL is not yet supported."

prettyPrint :: PPOptions Stage -> IO ()
prettyPrint PPOptions{..} = do
  bs <- BS.fromStrict . encodeUtf8 <$> readFile file
  case stage of
    VanillaLex ->
      case lex bs of
        Right tokens -> print tokens
        Left err -> panic . fromString $ err
    VanillaParse ->
      parseOrDie programParser bs $ \ast -> putStrLn
                            . pp
                            $ ast
    VanillaNormal ->
      parseOrDie programParser bs $ \ast -> putStrLn
                            . pp
                            . normalise
                            . nameQueries
                            $ ast
    Exalog ->
      parseOrDie programParser bs $ \ast -> do
        let (exalogProgram, initEDB) = compile
                                     . normalise
                                     . nameQueries
                                     $ ast
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
