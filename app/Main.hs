{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Protolude hiding (pred)

import qualified Data.ByteString.Lazy.Char8 as BS

import Text.PrettyPrint (render)

import qualified System.Console.Haskeline as HLine

import Options.Applicative hiding (command, header)

import qualified Language.Exalog.Core as E
import           Language.Exalog.Pretty ()
import qualified Language.Exalog.SrcLoc as Src
import qualified Language.Exalog.KnowledgeBase.Class as KB
import qualified Language.Exalog.KnowledgeBase.Knowledge as KB
import qualified Language.Exalog.KnowledgeBase.Set as KB
import           Language.Exalog.Provenance ()

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

  let stageEnv = S.defaultStageEnv
        { S._input = S.Textual (Src.File _file) bs S.SProgram
        , S._provenance = _provenance
        }
  ast <- succeedOrDie stageEnv S.parse
  evalOutput <- succeedOrDie stageEnv (S.solved mempty)

  case evalOutput of
    S.Simple  sol qPreds -> display ast qPreds sol
    S.Tracked sol qPreds -> display ast (E.peel <$> qPreds) (E.peel sol)

repl :: ReplOptions -> IO ()
repl ReplOptions{..} = do
  putStrLn @Text "Interactive Vanillalog environment - REPL"

  baseSolution <- maybe (pure mempty) computeBase _mFile

  HLine.runInputT HLine.defaultSettings (loop baseSolution)
  where
  computeBase file = do
    bs <- BS.fromStrict . encodeUtf8 <$> readFile file
    let stageEnv = S.defaultStageEnv
          { S._input = S.Textual (Src.File file) bs S.SProgram
          , S._keepPredicates = S.AllPreds
          }
    evalOutput <- succeedOrDie stageEnv (S.solved mempty)

    pure $ case evalOutput of
      S.Simple  solution _ -> solution
      S.Tracked solution _ -> E.peel solution

  loop baseSol = do
    mInput <- HLine.getInputLine prefix
    case mInput of
      Nothing -> pure ()
      Just input
        | input `elem` [ ":e", ":exit", ":q", ":quit" ] -> pure ()
        | otherwise -> do -- Interpret it as a query
          mOutput <- lift $
            S.runStage (mkReplEnv baseSol $ prefix <> input) (S.solved baseSol)
          HLine.outputStrLn $ case mOutput of
            Just output ->
              case output of
                S.Simple  sol _ -> render $ displayTuples (KB.toList sol)
                S.Tracked sol _ -> render $ displayTuples (KB.toList (E.peel sol))
            Nothing  -> "Ill-formed query. Try again."
          loop baseSol

  prefix = "?- "

  mkReplEnv :: KB.Set 'E.ABase -> [ Char ] -> S.StageEnv decl hop bop
  mkReplEnv sol inp = S.defaultStageEnv
    { S._input          = S.Textual
        { S._source      = BS.pack inp
        , S._inputSource = Src.None
        , S._parserScope = S.SSentence
        }
    , S._keepPredicates = S.OnlyQueryPreds
    , S._reservedNames  = reserved sol
    }

  reserved :: KB.Set 'E.ABase -> [ Text ]
  reserved sol = ((\E.Predicate{_predSym = E.PredicateSymbol txt} -> txt) E.$$)
             <$> KB.map (\KB.Knowledge{_predicate} -> E.PredicateBox _predicate) sol

prettyPrint :: PPOptions Stage -> IO ()
prettyPrint PPOptions{..} = do
  bs <- BS.fromStrict . encodeUtf8 <$> readFile _file
  let stageEnv =
        S.StageEnv (S.Textual (Src.File _file) bs S.SProgram) S.AllPreds [] False
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
