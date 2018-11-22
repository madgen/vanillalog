{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Vanillalog.Transformation.Normaliser (normalise) where

import Protolude

import Data.Functor.Foldable

import           Language.Vanillalog.AST
import qualified Language.Vanillalog.Generic.AST as G
import qualified Language.Vanillalog.Generic.Logger as L
import           Language.Vanillalog.Generic.Transformation.Util

normalise :: Program -> Either [ L.Error ] Program
normalise = L.runLogger
          . separateTopLevelDisjunctions
          . bubbleUpDisjunction
          . pushNegation

pushNegation :: Program -> Program
pushNegation = transform pnSub
  where
  pnSub :: Subgoal -> Subgoal
  pnSub = ana coalg

  coalg :: Coalgebra (Base Subgoal) Subgoal
  coalg (SNeg span (SConj _ sub1 sub2)) = SDisjF span (elimNeg $ SNeg span sub1) (elimNeg $ SNeg span sub2)
  coalg (SNeg span (SDisj _ sub1 sub2)) = SConjF span (elimNeg $ SNeg span sub1) (elimNeg $ SNeg span sub2)
  coalg s = project (elimNeg s)

-- | Repeatedly eliminates immediate double negation top-down but does not
-- traverse the tree all the way down if it sees a non-negation node.
elimNeg :: Subgoal -> Subgoal
elimNeg = apo rcoalg
  where
  rcoalg :: Subgoal -> Base Subgoal (Either Subgoal Subgoal)
  rcoalg (SNeg _ (SNeg _ sub)) = Left  <$> project sub
  rcoalg s                     = Right <$> project s

bubbleUpDisjunction :: Program -> Program
bubbleUpDisjunction = transform budSub
  where
  budSub :: Subgoal -> Subgoal
  budSub = cata alg

  alg :: Algebra (Base Subgoal) Subgoal
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
  G.Program _span <$> yakk _sentences
  where
  yakk :: [ Sentence ] -> L.LoggerM [ Sentence ]
  yakk = fix $ \f sol -> do
    newSol <- join <$> traverse step sol
    if sol == newSol then pure newSol else f newSol

  step :: Sentence -> L.LoggerM [ Sentence ]
  step fact@G.SFact{} = pure [ fact ]
  step sentence@G.SClause{..}
    | G.Clause _ head (SDisj s sub1 sub2) <- _clause =
      pure $ G.SClause _span <$> [ G.Clause s head sub1, G.Clause s head sub2 ]
    | otherwise = pure [ sentence ]
  step sentence@G.SQuery{..}
    | G.Query _ head@Just{} body <- _query =
      pure $ case body of
        SDisj s sub1 sub2 ->
          G.SQuery _span <$> [ G.Query s head sub1, G.Query s head sub2 ]
        _                 -> [ sentence ]
    | otherwise = L.scream L.Impossible (Just _span)
      "There should be no unnamed queries at this point."
