{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Vanillalog.Generic.Pretty
  ( pp
  , HasPrecedence(..)
  , Pretty(..)
  ) where

import Protolude hiding ((<>), empty, head)

import Data.Functor.Foldable (Base, para, project)
import Data.Text (unpack)

import Text.PrettyPrint

import Language.Exalog.Pretty.Helper

import Language.Vanillalog.Generic.AST

instance Pretty (Sentence op) => Pretty (Program op) where
  pretty (Program sentences) = vcat . prettyC $ sentences

instance (Pretty (Clause op), Pretty (Query op)) => Pretty (Sentence op) where
  pretty (SClause clause) = pretty clause
  pretty (SFact fact)     = pretty fact
  pretty (SQuery query)   = pretty query

instance Pretty (Subgoal op) => Pretty (Clause op) where
  pretty (Clause head body) =
    pretty head <+> ":-" <+> pretty body <> "."

instance Pretty Fact where
  pretty (Fact atom) = pretty atom <> "."

instance Pretty (Subgoal op) => Pretty (Query op) where
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
    alg s@(SUnOpF op (ch,doc)) =
      pretty op <> mParens (SomeOp op) (operation ch) doc
    alg s@(SBinOpF op (ch,doc) (ch',doc')) =
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

instance Pretty Text where
  pretty = text . unpack
