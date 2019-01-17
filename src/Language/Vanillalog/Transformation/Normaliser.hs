{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Vanillalog.Transformation.Normaliser (normalise) where

import Protolude

import Data.Functor.Foldable

import           Language.Vanillalog.AST
import qualified Language.Vanillalog.Generic.AST as G
import qualified Language.Vanillalog.Generic.Logger as L
import           Language.Vanillalog.Generic.Parser.SrcLoc (span)
import           Language.Vanillalog.Generic.Transformation.Util

normalise :: Program -> L.LoggerM Program
normalise = separateTopLevelDisjunctions
          . bubbleUpDisjunction
          . pushNegation

pushNegation :: Program -> Program
pushNegation = transformBody pnSub
  where
  pnSub :: Subgoal Op Term -> Subgoal Op Term
  pnSub = ana coalg

  coalg :: Coalgebra (Base (Subgoal Op Term)) (Subgoal Op Term)
  coalg (SNeg span (SConj _ sub1 sub2)) = SDisjF span (elimNeg $ SNeg span sub1) (elimNeg $ SNeg span sub2)
  coalg (SNeg span (SDisj _ sub1 sub2)) = SConjF span (elimNeg $ SNeg span sub1) (elimNeg $ SNeg span sub2)
  coalg s = project (elimNeg s)
-- | Repeatedly eliminates immediate double negation top-down but does not
-- traverse the tree all the way down if it sees a non-negation node.
elimNeg :: Subgoal Op Term -> Subgoal Op Term
elimNeg = apo rcoalg
  where
  rcoalg :: Subgoal Op Term
         -> Base (Subgoal Op Term) (Either (Subgoal Op Term) (Subgoal Op Term))
  rcoalg (SNeg _ (SNeg _ sub)) = Left  <$> project sub
  rcoalg s                     = Right <$> project s

bubbleUpDisjunction :: Program -> Program
bubbleUpDisjunction = transformBody budSub
  where
  budSub :: Subgoal Op Term -> Subgoal Op Term
  budSub = cata alg

  alg :: Algebra (Base (Subgoal Op Term)) (Subgoal Op Term)
  alg (SConjF span (SDisj _ sub1 sub2) (SDisj _ sub3 sub4)) =
    SDisj span (SConj span sub1 sub3)
               (SDisj span (SConj span sub1 sub4)
                           (SDisj span (SConj span sub2 sub3)
                                       (SConj span sub2 sub4)))
  alg (SConjF span (SDisj _ sub1 sub2) sub3) = SDisj span (SConj span sub1 sub3) (SConj span sub2 sub3)
  alg (SConjF span sub1 (SDisj _ sub2 sub3)) = SDisj span (SConj span sub1 sub2) (SConj span sub1 sub3)
  alg s = embed s

separateTopLevelDisjunctions :: Program -> L.LoggerM Program
separateTopLevelDisjunctions G.Program{..} =
  G.Program _span <$> yakk _statements
  where
  yakk :: [ Statement ] -> L.LoggerM [ Statement ]
  yakk = fix $ \f sol -> do
    newSol <- join <$> traverse step sol
    if sol == newSol then pure newSol else f newSol

  step :: Statement -> L.LoggerM [ Statement ]
  step G.StSentence{..} =
    fmap (\sent -> G.StSentence (span sent) sent) <$> step' _sentence
  step decl@G.StDeclaration{} = pure [ decl ]

  step' :: Sentence -> L.LoggerM [ Sentence ]
  step' fact@G.SFact{} = pure [ fact ]
  step' sentence@G.SClause{..}
    | G.Clause _ head (SDisj s sub1 sub2) <- _clause =
      pure $ G.SClause <$> [ G.Clause s head sub1, G.Clause s head sub2 ]
    | otherwise = pure [ sentence ]
  step' sentence@G.SQuery{..}
    | G.Query _ head@Just{} body <- _query =
      pure $ case body of
        SDisj s sub1 sub2 ->
          G.SQuery <$> [ G.Query s head sub1, G.Query s head sub2 ]
        _                 -> [ sentence ]
    | otherwise = L.scream (Just _span)
      "There should be no unnamed queries at this point."
