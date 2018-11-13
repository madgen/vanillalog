{-# LANGUAGE FlexibleInstances #-}

module Language.Vanillalog.Pretty (pp) where

import Protolude hiding ((<>), empty, head)

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Functor.Foldable (Base, para, project)

import Text.PrettyPrint

import Language.Exalog.Pretty.Helper

import Language.Vanillalog.AST

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

instance Pretty Subgoal where
  pretty = para alg
    where
    basePrecedence :: Base Subgoal a -> Int
    basePrecedence SAtomF{} = 1
    basePrecedence SNegF{}  = 1
    basePrecedence SConjF{} = 2
    basePrecedence SDisjF{} = 3

    alg :: Base Subgoal (Subgoal, Doc) -> Doc
    alg (SAtomF atom) = pretty atom
    alg s@(SNegF (ch,doc)) =
      "!" <> mParens s ch doc
    alg s@(SConjF (ch,doc) (ch',doc')) =
      mParens s ch doc <> comma <+> mParens s ch' doc'
    alg s@(SDisjF (ch,doc) (ch',doc')) =
      mParens s ch doc <> semi <+> mParens s ch' doc'

    mParens :: Base Subgoal a -> Subgoal -> Doc -> Doc
    mParens s ch doc =
      if basePrecedence s < (basePrecedence . project $ ch)
        then parens doc
        else doc

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
