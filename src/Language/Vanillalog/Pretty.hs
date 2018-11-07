{-# LANGUAGE FlexibleInstances #-}

module Language.Vanillalog.Pretty (pp) where

import Protolude hiding ((<>), empty, head)

import qualified Data.ByteString.Lazy.Char8 as BS

import Text.PrettyPrint

import Language.Exalog.Pretty.Helper

import Language.Vanillalog.AST

instance Pretty Program where
  pretty (Program clauses) = vcat . prettyC $ clauses

instance Pretty (Either Clause Fact) where
  pretty (Left cl) = pretty cl
  pretty (Right fact) = pretty fact

instance Pretty Clause where
  pretty (Clause head body) =
    pretty head <+> ":-" <+> (pretty body) <> "."

instance Pretty Fact where
  pretty (Fact atom) = pretty atom <> "."

instance Pretty Subgoal where
  pretty (SAtom atom) = pretty atom
  pretty (SNeg sub) = "!" <> parens (pretty sub)
  pretty (SComma sub1 sub2) = pretty sub1 <> comma <+> pretty sub2

instance Pretty AtomicFormula where
  pretty (AtomicFormula name terms) =
    pretty name <> parens (csep . prettyC $ terms)

instance Pretty Term where
  pretty (TVar v) = pretty v
  pretty (TSym s) = pretty s

instance Pretty Var where
  pretty (Var v) = pretty v

instance Pretty Sym where
  pretty (SInt i) = int i
  pretty (SText bs) = doubleQuotes $ pretty bs
  pretty (SBool True) = "true"
  pretty (SBool False) = "false"

-- Generic instances

instance Pretty BS.ByteString where
  pretty = text . BS.unpack
