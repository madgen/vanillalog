{-# LANGUAGE GADTs #-}
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

peephole :: (VanillaSubgoal -> VanillaSubgoal) -> Program -> Program
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
  pnSub :: VanillaSubgoal -> VanillaSubgoal
  pnSub = cata alg

  alg :: Algebra (Base VanillaSubgoal) VanillaSubgoal
  alg (SUnOpF Negation sub)
    | SBinOp op sub1 sub2 <- sub
    , op == Conjunction || op == Disjunction =
      SBinOp (if op == Conjunction then Disjunction else Conjunction)
        (pnSub $ SUnOp Negation sub1)
        (pnSub $ SUnOp Negation sub2)
  alg s = embed $ fmap pnSub s

elimDoubleNegation :: Program -> Program
elimDoubleNegation = peephole ednSub
  where
  ednSub :: VanillaSubgoal -> VanillaSubgoal
  ednSub = cata alg

  alg :: Algebra (Base VanillaSubgoal) VanillaSubgoal
  alg (SUnOpF Negation (SUnOp Negation sub)) = sub
  alg s = embed s

bubbleUpDisjunction :: Program -> Program
bubbleUpDisjunction = peephole budSub
  where
  budSub :: VanillaSubgoal -> VanillaSubgoal
  budSub = cata alg

  alg :: Algebra (Base VanillaSubgoal) VanillaSubgoal
  alg (SBinOpF Conjunction (SBinOp Disjunction s1 s2) s3) =
    SBinOp Disjunction
      (SBinOp Conjunction s1 s3)
      (SBinOp Conjunction s2 s3)
  alg (SBinOpF Conjunction s1 (SBinOp Disjunction s2 s3)) =
    SBinOp Disjunction
      (SBinOp Conjunction s1 s2)
      (SBinOp Conjunction s1 s3)
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
    | Clause head (SBinOp Disjunction s1 s2) <- clause =
      SClause <$> [ Clause head s1, Clause head s2 ]
    | otherwise = [ sentence ]
  step sentence@(SQuery query)
    | Query head@(Just{}) body <- query =
      case body of
        SBinOp Disjunction s1 s2 -> SQuery <$> [ Query head s1, Query head s2 ]
        _ -> [ sentence ]
    | otherwise = panic
      "Impossible: There should be no unnamed queries at this point."
