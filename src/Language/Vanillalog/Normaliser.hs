{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Vanillalog.Normaliser (normalise) where

import Protolude

import Data.Functor.Foldable

import           Language.Vanillalog.AST
import qualified Language.Vanillalog.AST.Generic as G

type Algebra f a = f a -> a

normalise :: Program -> Program
normalise = separateTopLevelDisjunctions
          . bubbleUpDisjunction
          . elimDoubleNegation
          . pushNegation

peephole :: (Subgoal -> Subgoal) -> Program -> Program
peephole transformation (G.Program sentences) =
  G.Program (map goS sentences)
  where
  goS :: Sentence -> Sentence
  goS (G.SClause G.Clause{..}) = G.SClause G.Clause{body = transformation body, ..}
  goS (G.SQuery  G.Query{..})  = G.SQuery  G.Query{body = transformation body, ..}
  goS s = s

pushNegation :: Program -> Program
pushNegation = peephole pnSub
  where
  pnSub :: Subgoal -> Subgoal
  pnSub = cata alg

  alg :: Algebra (Base Subgoal) Subgoal
  alg (SNegF (SConj sub1 sub2)) = SDisj (pnSub $ SNeg sub1) (pnSub $ SNeg sub2)
  alg (SNegF (SDisj sub1 sub2)) = SConj (pnSub $ SNeg sub1) (pnSub $ SNeg sub2)
  alg s = embed $ fmap pnSub s

elimDoubleNegation :: Program -> Program
elimDoubleNegation = peephole ednSub
  where
  ednSub :: Subgoal -> Subgoal
  ednSub = cata alg

  alg :: Algebra (Base Subgoal) Subgoal
  alg (SNegF (SNeg sub)) = sub
  alg s = embed s

bubbleUpDisjunction :: Program -> Program
bubbleUpDisjunction = peephole budSub
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

separateTopLevelDisjunctions :: Program -> Program
separateTopLevelDisjunctions (G.Program sentences) =
  G.Program (yakk sentences)
  where
  yakk :: [ Sentence ] -> [ Sentence ]
  yakk = fix
    (\f sol ->
      let newSol = join . map step $ sol
      in if sol == newSol then newSol else f newSol)

  step :: Sentence -> [ Sentence ]
  step fact@G.SFact{} = [ fact ]
  step sentence@(G.SClause clause)
    | G.Clause head (SDisj s1 s2) <- clause =
      G.SClause <$> [ G.Clause head s1, G.Clause head s2 ]
    | otherwise = [ sentence ]
  step sentence@(G.SQuery query)
    | G.Query head@(Just{}) body <- query =
      case body of
        SDisj s1 s2 -> G.SQuery <$> [ G.Query head s1, G.Query head s2 ]
        _ -> [ sentence ]
    | otherwise = panic
      "Impossible: There should be no unnamed queries at this point."
