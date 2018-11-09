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
peephole transformation (Program clausesNFacts) =
  Program (map (first pnClause) clausesNFacts)
  where
  pnClause :: Clause -> Clause
  pnClause Clause{..} = Clause{body = transformation body, ..}

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
separateTopLevelDisjunctions (Program clausesNFacts) =
  Program (yakk $ clausesNFacts)
  where
  yakk :: [ Either Clause Fact ] -> [ Either Clause Fact ]
  yakk = fix
    (\f sol ->
      let newSol = join . map step $ sol
      in if sol == newSol then newSol else f newSol)

  step :: Either Clause Fact -> [ Either Clause Fact ]
  step fact@Right{} = [ fact ]
  step (Left (Clause head (SDisj s1 s2))) =
    [ Left (Clause head s1)
    , Left (Clause head s2) ]
  step cl@Left{} = [ cl ]
