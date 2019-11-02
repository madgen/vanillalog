{-# LANGUAGE DuplicateRecordFields #-}

module Language.Vanillalog.Generic.CLI.Arguments where

import Protolude

import Options.Applicative

newtype RunOptions = RunOptions
  { _file :: FilePath }

newtype ReplOptions = ReplOptions
  { _mFile :: Maybe FilePath }

data PPOptions stage = PPOptions
  { _file  :: FilePath
  , _stage :: stage
  }

fileParser :: Parser FilePath
fileParser = strOption
  ( long "file"
 <> short 'f'
 <> metavar "FILENAME"
 <> help "Input file"
  )

stageFlag' :: stage -> [ Char ] -> [ Char ] -> Parser stage
stageFlag' stage longName helpMsg = flag' stage (long longName <> help helpMsg)

fromStageParser :: Parser stage -> Parser (PPOptions stage)
fromStageParser stageParser = PPOptions <$> fileParser <*> stageParser

runOptions :: Parser RunOptions
runOptions = RunOptions <$> fileParser

replOptions :: Parser ReplOptions
replOptions = ReplOptions <$> optional fileParser

data Command stage =
  Run RunOptions | Repl ReplOptions | PrettyPrint (PPOptions stage)

-- | Overall option parser
optsParser :: Parser (PPOptions stage) -> [ Char ] -> ParserInfo (Command stage)
optsParser ppOptions headerStr = info (hsubparser
  ( command "run"  (info (Run <$> runOptions)        (progDesc "Run a Datalog program"))
 <> command "repl" (info (Repl <$> replOptions)      (progDesc "Read-eval-print loop"))
 <> command "pp"   (info (PrettyPrint <$> ppOptions) (progDesc "Pretty print"))
  ) <**> helper) generalInfo
  where
  generalInfo = fullDesc <> header headerStr
