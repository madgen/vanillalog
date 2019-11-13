{-# LANGUAGE DataKinds #-}

module Language.Vanillalog.DSL
  ( Datalog
  , runDatalog
  , (/\)
  , (\/)
  , neg
  , module GDSL
  ) where

import Protolude hiding ((.))

import Language.Exalog.SrcLoc (SrcSpan(NoSpan))
import qualified Language.Exalog.Relation as R
import qualified Language.Exalog.Core as E

import           Language.Vanillalog.Stage (solved)
import qualified Language.Vanillalog.Generic.Stage as S
import           Language.Vanillalog.AST
import qualified Language.Vanillalog.Generic.AST as AG
import           Language.Vanillalog.Generic.DSL as GDSL

type Datalog = GenericDatalog Void (Const Void) Op

runDatalog :: GenericDatalog Void (Const Void) Op -> IO (Maybe (R.Solution 'E.ABase))
runDatalog statements = S.runStage dslStageEnv (solved mempty)
  where
  program = AG.Program NoSpan (statements [])
  dslStageEnv = (S.defaultStageEnv {S._input = S.AST program})

infixl 3 /\
(/\) :: Subgoal Op Term -> Subgoal Op Term -> Subgoal Op Term
sub1 /\ sub2 = SConj NoSpan sub1 sub2

infixl 3 \/
(\/) :: Subgoal Op Term -> Subgoal Op Term -> Subgoal Op Term
sub1 \/ sub2 = SDisj NoSpan sub1 sub2

neg :: Subgoal Op Term -> Subgoal Op Term
neg = SNeg NoSpan
