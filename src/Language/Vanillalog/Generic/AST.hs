{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Vanillalog.Generic.AST where

import Protolude

import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)

import Language.Vanillalog.Generic.Parser.SrcLoc

data Program decl op = Program
  { _span       :: SrcSpan
  , _statements :: [ Statement decl op ]
  }

data Statement decl op =
    StDeclaration { _span :: SrcSpan, _declaration :: decl }
  | StSentence    { _span :: SrcSpan, _sentence    :: Sentence op }

deriving instance
  (Eq decl, Eq (op 'Nullary), Eq (op 'Unary), Eq (op 'Binary))
    => Eq (Statement decl op)

data Sentence op =
    SClause { _span :: SrcSpan, _clause :: Clause op }
  | SFact   { _span :: SrcSpan, _fact   :: Fact      }
  | SQuery  { _span :: SrcSpan, _query  :: Query op  }

deriving instance
  (Eq (op 'Nullary), Eq (op 'Unary), Eq (op 'Binary)) => Eq (Sentence op)

data Query op = Query
  { _span :: SrcSpan
  , _head :: Maybe (AtomicFormula Var)
  , _body :: Subgoal op
  }

deriving instance
  (Eq (op 'Nullary), Eq (op 'Unary), Eq (op 'Binary)) => Eq (Query op)

data Clause op = Clause
  { _span :: SrcSpan
  , _head :: AtomicFormula Term
  , _body :: Subgoal op
  }

deriving instance
  (Eq (op 'Nullary), Eq (op 'Unary), Eq (op 'Binary)) => Eq (Clause op)

data Fact = Fact
  { _span :: SrcSpan
  , _atom :: AtomicFormula Sym
  } deriving (Eq)

data Subgoal op =
    SAtom
      { _span :: SrcSpan
      , _atom :: AtomicFormula Term
      }
  | SNullOp
      { _span   :: SrcSpan
      , _nullOp :: op 'Nullary
      }
  | SUnOp
      { _span  :: SrcSpan
      , _unOp  :: op 'Unary
      , _child :: Subgoal op
      }
  | SBinOp
      { _span   :: SrcSpan
      , _binOp  :: op 'Binary
      , _child1 :: Subgoal op
      , _child2 :: Subgoal op
      }

deriving instance
  (Eq (op 'Nullary), Eq (op 'Unary), Eq (op 'Binary)) => Eq (Subgoal op)

data SomeOp (op :: OpKind -> *) = NoOp | forall opKind . SomeOp (op opKind)

data OpKind = Binary | Unary | Nullary

data AtomicFormula a =
  AtomicFormula
    { _span  :: SrcSpan
    , _fxSym :: Text
    , _terms :: [ a ]
    } deriving (Eq, Functor, Foldable, Traversable)

data Term =
    TVar { _var :: Var }
  | TSym { _sym :: Sym }
  deriving (Eq)

data TermType = TTInt | TTText | TTBool deriving (Eq)

termType :: Sym -> TermType
termType SymInt{}  = TTInt
termType SymText{} = TTText
termType SymBool{} = TTBool

data Var = Var SrcSpan Text deriving (Eq)
data Sym =
    SymInt  { _span :: SrcSpan, _int  :: Int  }
  | SymText { _span :: SrcSpan, _text :: Text }
  | SymBool { _span :: SrcSpan, _bool :: Bool }
  deriving (Eq)

makeBaseFunctor ''Subgoal

operation :: Subgoal op -> SomeOp op
operation SAtom{}     = NoOp
operation s@SNullOp{} = SomeOp (_nullOp s)
operation s@SUnOp{}   = SomeOp (_unOp s)
operation s@SBinOp{}  = SomeOp (_binOp s)

atoms :: Subgoal op -> [ AtomicFormula Term ]
atoms = cata alg
  where
  alg :: Base (Subgoal op) [ AtomicFormula Term ] -> [ AtomicFormula Term ]
  alg s@SAtomF{..} = [ _atomF ]
  alg SNullOpF{}   = []
  alg SUnOpF{..}   = _childF
  alg SBinOpF{..}  = _child1F ++ _child2F

vars :: Subgoal a -> [ Var ]
vars = cata alg
  where
  alg :: Base (Subgoal a) [ Var ] -> [ Var ]
  alg (SAtomF _ (AtomicFormula _ _ terms)) =
    mapMaybe (\case {TVar v -> Just v; _ -> Nothing}) terms
  alg (SUnOpF _ _ vars) = vars
  alg (SBinOpF _ _ vars1 vars2) = vars1 ++ vars2
