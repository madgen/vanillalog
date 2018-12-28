{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
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

instance (Pretty decl, Pretty (Sentence op)) => Pretty (Program decl op) where
  pretty Program{..} = vcat . prettyC $ _statements

instance (Pretty decl, Pretty (Sentence op)) => Pretty (Statement decl op) where
  pretty StSentence{..}    = pretty _sentence
  pretty StDeclaration{..} = pretty _declaration

instance (Pretty (Clause op), Pretty (Query op)) => Pretty (Sentence op) where
  pretty SClause{..} = pretty _clause
  pretty SFact{..}   = pretty _fact
  pretty SQuery{..}  = pretty _query

instance Pretty (Subgoal op) => Pretty (Clause op) where
  pretty Clause{..} =
    pretty _head <+> ":-" <+> pretty _body <> "."

instance Pretty Fact where
  pretty Fact{..} = pretty _atom <> "."

instance Pretty (Subgoal op) => Pretty (Query op) where
  pretty Query{..} =
    case _head of { Just head-> pretty head; _ -> empty }
    <+>  "?-" <+> pretty _body <> "."

instance ( Pretty (op 'Unary)
         , Pretty (op 'Binary)
         , HasPrecedence op
         ) => Pretty (Subgoal op) where
  pretty = para alg
    where
    alg :: Base (Subgoal op) (Subgoal op, Doc) -> Doc
    alg (SAtomF _ atom) = pretty atom
    alg s@(SUnOpF _ op (ch,doc)) =
      pretty op <> mParens (SomeOp op) (operation ch) doc
    alg s@(SBinOpF _ op (ch,doc) (ch',doc')) =
         mParens (SomeOp op) (operation ch) doc
      <> pretty op
      <> mParens (SomeOp op) (operation ch') doc'

    mParens :: SomeOp op -> SomeOp op -> Doc -> Doc
    mParens op1 op2 doc =
      if precedence op1 < precedence op2
        then parens doc
        else doc

class HasPrecedence (op :: OpKind -> *) where
  precedence :: SomeOp op -> Int

instance Pretty a => Pretty (AtomicFormula a) where
  pretty AtomicFormula{..} =
    pretty _fxSym <> parens (csep . prettyC $ _terms)

instance Pretty Term where
  pretty TVar{ _var = v } = pretty v
  pretty TSym{ _sym = s } = pretty s

instance Pretty Var where
  pretty (Var _ v) = pretty v

instance Pretty Sym where
  pretty (SymInt  _ i)     = int i
  pretty (SymText _ bs)    = doubleQuotes $ pretty bs
  pretty (SymBool _ True)  = "true"
  pretty (SymBool _ False) = "false"

-- Generic instances

instance Pretty Text where
  pretty = text . unpack
