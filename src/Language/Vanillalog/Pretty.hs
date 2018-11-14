{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Vanillalog.Pretty (pp) where

import Protolude hiding ((<>), empty, head)

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Functor.Foldable (Base, para, project)

import Text.PrettyPrint

import Language.Exalog.Pretty.Helper

import           Language.Vanillalog.AST
import qualified Language.Vanillalog.AST.Generic as AG

instance Pretty Program where
  pretty (Program sentences) = vcat . prettyC $ sentences

instance Pretty Sentence where
  pretty (SClause clause) = pretty clause
  pretty (SFact fact)     = pretty fact
  pretty (SQuery query)   = pretty query

instance Pretty Clause where
  pretty (Clause head body) =
    pretty head <+> ":-" <+> pretty body <> "."

instance Pretty Fact where
  pretty (Fact atom) = pretty atom <> "."

instance Pretty Query where
  pretty (Query mHead body) =
    case mHead of { Just head -> pretty head; _ -> empty }
    <+>  "?-" <+> pretty body <> "."

instance ( Pretty (op 'Unary)
         , Pretty (op 'Binary)
         , HasPrecedence op
         ) => Pretty (Subgoal op) where
  pretty = para alg
    where
    alg :: Base (Subgoal op) ((Subgoal op), Doc) -> Doc
    alg (SAtomF atom) = pretty atom
    alg s@(AG.SUnOpF op (ch,doc)) =
      pretty op <> mParens (SomeOp op) (operation ch) doc
    alg s@(AG.SBinOpF op (ch,doc) (ch',doc')) =
          mParens (SomeOp op) (operation ch) doc
       <> pretty op
      <+> mParens (SomeOp op) (operation ch') doc'

    mParens :: SomeOp op -> SomeOp op -> Doc -> Doc
    mParens op1 op2 doc =
      if precedence op1 < precedence op2
        then parens doc
        else doc

class HasPrecedence (op :: OpKind -> *) where
  precedence :: SomeOp op -> Int

instance HasPrecedence Op where
  precedence NoOp                 = 0
  precedence (SomeOp Negation)    = 1
  precedence (SomeOp Conjunction) = 2
  precedence (SomeOp Disjunction) = 3

instance Pretty (Op opKind) where
  pretty Negation    = "!"
  pretty Conjunction = ","
  pretty Disjunction = ";"

instance Pretty AtomicFormula where
  pretty (AtomicFormula name terms) =
    pretty name <> parens (csep . prettyC $ terms)

instance Pretty Term where
  pretty (TVar v) = pretty v
  pretty (TSym s) = pretty s

instance Pretty Var where
  pretty (Var v) = pretty v

instance Pretty Sym where
  pretty (SymInt i) = int i
  pretty (SymText bs) = doubleQuotes $ pretty bs
  pretty (SymBool True) = "true"
  pretty (SymBool False) = "false"

-- Generic instances

instance Pretty BS.ByteString where
  pretty = text . BS.unpack
