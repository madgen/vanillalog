{-# LANGUAGE RecordWildCards #-}

module Language.Vanillalog.Normaliser (normalise) where

import Protolude

import Data.Functor.Foldable

import Language.Vanillalog.AST

type Algebra f a = f a -> a

normalise :: Program -> Program
normalise = separateTopLevelDisjunctions
          . bubbleUpDisjunction
          . elimDoubleNegation
          . pushNegation

peephole :: (Subgoal -> Subgoal) -> Program -> Program
peephole transformation (Program sentences) =
  Program (map goS sentences)
  where
  goS :: Sentence -> Sentence
  goS (SClause Clause{..}) = SClause Clause{body = transformation body, ..}
  goS (SQuery  Query{..})  = SQuery  Query{body = transformation body, ..}
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
  ednSub = alg . project

  alg :: Algebra (Base Subgoal) Subgoal
  alg (SNegF (SNeg sub)) = ednSub sub
  alg s = embed $ fmap ednSub s

bubbleUpDisjunction :: Program -> Program
bubbleUpDisjunction = peephole budSub
  where
  budSub :: Subgoal -> Subgoal
  budSub = cata alg

  alg :: Algebra (Base Subgoal) Subgoal
  alg (SConjF (SDisj s1 s2) s3) = SDisj (SConj s1 s3) (SConj s2 s3)
  alg (SConjF s1 (SDisj s2 s3)) = SDisj (SConj s1 s2) (SConj s1 s3)
  alg s = embed s

separateTopLevelDisjunctions :: Program -> Program
separateTopLevelDisjunctions (Program sentences) =
  Program (yakk sentences)
  where
  yakk :: [ Sentence ] -> [ Sentence ]
  yakk = fix
    (\f sol ->
      let newSol = join . map step $ sol
      in if sol == newSol then newSol else f newSol)

  step :: Sentence -> [ Sentence ]
  step fact@SFact{} = [ fact ]
  step sentence@(SClause clause)
    | Clause head (SDisj s1 s2) <- clause =
      SClause <$> [ Clause head s1, Clause head s2 ]
    | otherwise = [ sentence ]
  step sentence@(SQuery query)
    | Query head@(Just{}) body <- query =
      case body of
        SDisj s1 s2 -> SQuery <$> [ Query head s1, Query head s2 ]
        _ -> [ sentence ]
    | otherwise = panic
      "Impossible: There should be no unnamed queries at this point."
