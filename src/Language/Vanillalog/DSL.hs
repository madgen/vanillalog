{-# LANGUAGE DataKinds #-}

module Language.Vanillalog.DSL
  ( Datalog
  , runDatalog
  , (/\)
  , (\/)
  , neg
  , module GDSL
  ) where

import Protolude

import qualified Language.Exalog.Core as E
import qualified Language.Exalog.Logger as Log
import qualified Language.Exalog.KnowledgeBase.Set as KB
import           Language.Exalog.SrcLoc (SrcSpan(NoSpan))

import           Language.Vanillalog.Stage (EvaluationOutput(..), solved)
import qualified Language.Vanillalog.Generic.Stage as S
import           Language.Vanillalog.AST
import           Language.Vanillalog.Generic.DSL as GDSL

type Datalog = GenericDatalogT Void (Const Void) Op (S.StageT Void (Const Void) Op Log.Logger)

runDatalog :: Datalog -> IO (Maybe (KB.Set 'E.ABase))
runDatalog datalog = S.runStage S.defaultStageEnv $ do
  program <- genDatalogT datalog
  output <- local (\s -> s {S._input = S.AST program}) $ solved mempty
  case output of
    Simple solution _ -> pure solution
    Tracked{} -> lift $
      Log.scream NoSpan "The DSL cannot be run in the provenance mode."

infixl 3 /\
(/\) :: Subgoal Op Term -> Subgoal Op Term -> Subgoal Op Term
sub1 /\ sub2 = SConj NoSpan sub1 sub2

infixl 3 \/
(\/) :: Subgoal Op Term -> Subgoal Op Term -> Subgoal Op Term
sub1 \/ sub2 = SDisj NoSpan sub1 sub2

neg :: Subgoal Op Term -> Subgoal Op Term
neg = SNeg NoSpan
