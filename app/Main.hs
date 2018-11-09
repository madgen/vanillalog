{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Protolude hiding (fromStrict)

import Data.ByteString.Lazy.Char8 (fromStrict)
import Data.String (fromString)

import           Language.Exalog.Pretty ()
import qualified Language.Exalog.Solver as S

import Language.Vanillalog.Compiler (compile)
import Language.Vanillalog.Normaliser (normalise)
import Language.Vanillalog.Pretty (pp)
import Language.Vanillalog.Parser.Lexer (lex)
import Language.Vanillalog.Parser.Parser (programParser)

import Options.Applicative

data RunOptions = RunOptions
  { file :: FilePath }

data ReplOptions = ReplOptions

data PPOptions = PPOptions
  { file  :: FilePath
  , stage :: Stage
  }

data Stage = VanillaLex | VanillaParse | VanillaNormal | Exalog

-- Little option parsers

fileParser :: Parser FilePath
fileParser = strOption
  ( long "file"
 <> short 'f'
 <> metavar "FILENAME"
 <> help "Input file"
  )

runOptions :: Parser RunOptions
runOptions = RunOptions <$> fileParser

replOptions :: Parser ReplOptions
replOptions = pure ReplOptions

ppOptions :: Parser PPOptions
ppOptions = PPOptions <$> fileParser <*>
   ( flag' VanillaParse (long "vanilla-parse" <> help "Parse of the input")
 <|>
     flag' VanillaLex (long "vanilla-lex" <> help "Tokens of the input")
 <|>
     flag' VanillaNormal (long "vanilla-normal" <> help "Normal form")
 <|>
     flag' Exalog (long "exalog" <> help "For the compiled exalog program")
   )

data Command = Run RunOptions | Repl ReplOptions | PrettyPrint PPOptions

-- | Overall option parser
opts :: Parser Command
opts = subparser
  ( command "run"  (info (Run <$> runOptions) (progDesc "Run Datalog"))
 <> command "repl" (info (Repl <$> replOptions) (progDesc "REPL"))
 <> command "pp"   (info (PrettyPrint <$> ppOptions) (progDesc "Pretty print"))
  )

-- Functionality to run

run :: RunOptions-> IO ()
run RunOptions{..} = do
  bs <- fromStrict . encodeUtf8 <$> readFile file
  case programParser bs of
    Right ast -> do
      let (exalogProgram, initEDB) = compile . normalise $ ast
      finalEDB <- S.solve exalogProgram initEDB
      putStrLn $ pp finalEDB
    Left err -> panic . fromString $ err

repl :: ReplOptions -> IO ()
repl opts = panic "REPL is not yet supported."

prettyPrint :: PPOptions -> IO ()
prettyPrint PPOptions{..} = do
  bs <- fromStrict . encodeUtf8 <$> readFile file
  case stage of
    VanillaLex ->
      case lex bs of
        Right tokens -> print tokens
        Left err -> panic . fromString $ err
    VanillaParse ->
      case programParser bs of
        Right ast -> putStrLn . pp $ ast
        Left err -> panic . fromString $ err
    VanillaNormal ->
      case programParser bs of
        Right ast -> putStrLn . pp . normalise $ ast
        Left err -> panic . fromString $ err
    Exalog ->
      case programParser bs of
        Right ast -> do
          let (exalogProgram, initEDB) = compile ast
          putStrLn $ pp exalogProgram
          putStrLn ("" :: Text)
          putStrLn $ pp initEDB
        Left err -> panic . fromString $ err

main :: IO ()
main = do
  command <- execParser (info opts idm)
  case command of
    Run runOpts   -> run  runOpts
    Repl replOpts -> repl replOpts
    PrettyPrint ppOpts -> prettyPrint ppOpts
