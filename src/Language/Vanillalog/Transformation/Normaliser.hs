{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Vanillalog.Transformation.Normaliser (normalise) where

import Protolude

import Data.Functor.Foldable

import Control.Monad.Trans.Writer (Writer, runWriter, tell)

import           Language.Vanillalog.AST
import qualified Language.Vanillalog.Generic.AST as G
import           Language.Vanillalog.Generic.Transformation.Util

normalise :: Program -> Either [ Text ] Program
normalise pr =
  case runWriter $ separateTopLevelDisjunctions
                 . bubbleUpDisjunction
                 . pushNegation
                 $ pr of
    (pr', errs) | null errs -> Right pr'
                | otherwise -> Left errs

pushNegation :: Program -> Program
pushNegation = transform pnSub
  where
  pnSub :: Subgoal -> Subgoal
  pnSub = ana coalg

  coalg :: Coalgebra (Base Subgoal) Subgoal
  coalg (SNeg (SConj s1 s2)) = SDisjF (elimNeg $ SNeg s1) (elimNeg $ SNeg s2)
  coalg (SNeg (SDisj s1 s2)) = SConjF (elimNeg $ SNeg s1) (elimNeg $ SNeg s2)
  coalg s = project (elimNeg s)

-- | Repeatedly eliminates immediate double negation top-down but does not
-- traverse the tree all the way down if it sees a non-negation node.
elimNeg :: Subgoal -> Subgoal
elimNeg = apo rcoalg
  where
  rcoalg :: Subgoal -> Base Subgoal (Either Subgoal Subgoal)
  rcoalg (SNeg (SNeg sub)) = Left  <$> project sub
  rcoalg s                 = Right <$> project s

bubbleUpDisjunction :: Program -> Program
bubbleUpDisjunction = transform budSub
  where
  budSub :: Subgoal -> Subgoal
  budSub = cata alg

  alg :: Algebra (Base Subgoal) Subgoal
  alg (SConjF (SDisj s1 s2) (SDisj s3 s4)) =
    SDisj (SConj s1 s3)
          (SDisj (SConj s1 s4)
                 (SDisj (SConj s2 s3)
                        (SConj s2 s4)))
  alg (SConjF (SDisj s1 s2) s3) = SDisj (SConj s1 s3) (SConj s2 s3)
  alg (SConjF s1 (SDisj s2 s3)) = SDisj (SConj s1 s2) (SConj s1 s3)
  alg s = embed s

separateTopLevelDisjunctions :: Program -> Writer [ Text ] Program
separateTopLevelDisjunctions (G.Program sentences) =
  G.Program <$> yakk sentences
  where
  yakk :: [ Sentence ] -> Writer [ Text ] [ Sentence ]
  yakk = fix $ \f sol -> do
    newSol <- join <$> traverse step sol
    if sol == newSol then pure newSol else f newSol

  step :: Sentence -> Writer [ Text ] [ Sentence ]
  step fact@G.SFact{} = pure $ [ fact ]
  step sentence@(G.SClause clause)
    | G.Clause head (SDisj s1 s2) <- clause =
      pure $ G.SClause <$> [ G.Clause head s1, G.Clause head s2 ]
    | otherwise = pure [ sentence ]
  step sentence@(G.SQuery query)
    | G.Query head@(Just{}) body <- query =
      pure $ case body of
        SDisj s1 s2 -> G.SQuery <$> [ G.Query head s1, G.Query head s2 ]
        _           -> [ sentence ]
    | otherwise = do
      tell [ "Impossible: There should be no unnamed queries at this point." ]
      pure [ sentence ]
