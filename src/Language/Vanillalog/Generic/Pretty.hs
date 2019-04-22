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

import Data.Functor.Foldable (Base, para)

import Text.PrettyPrint

import Language.Exalog.Pretty ()
import Language.Exalog.Pretty.Helper

import Language.Vanillalog.Generic.AST

instance ( Pretty decl, Pretty (Sentence hop bop)
         ) => Pretty (Program decl hop bop) where
  pretty Program{..} = vcat . prettyC $ _statements

instance ( Pretty decl, Pretty (Sentence hop bop)
         ) => Pretty (Statement decl hop bop) where
  pretty StSentence{..}    = pretty _sentence
  pretty StDeclaration{..} = pretty _declaration

instance ( Pretty (Clause hop bop)
         , Pretty (Query hop bop)
         , Pretty (Fact hop)
         ) => Pretty (Sentence hop bop) where
  pretty SClause{..} = pretty _clause
  pretty SFact{..}   = pretty _fact
  pretty SQuery{..}  = pretty _query

instance ( Pretty (Subgoal hop Term)
         , Pretty (Subgoal bop Term)
         ) => Pretty (Clause hop bop) where
  pretty Clause{..} =
    pretty _head <+> ":-" <+> pretty _body <> "."

instance Pretty (Subgoal hop Term) => Pretty (Fact hop) where
  pretty Fact{..} = pretty _head <> "."

instance ( Pretty (Subgoal hop Var)
         , Pretty (Subgoal bop Term)
         ) => Pretty (Query hop bop) where
  pretty Query{..} =
    case _head of { Just head-> pretty head; _ -> empty }
    <+>  "?-" <+> pretty _body <> "."

instance ( Pretty (op 'Nullary)
         , Pretty (op 'Unary)
         , Pretty (op 'Binary)
         , Pretty term
         , HasPrecedence op
         ) => Pretty (Subgoal op term) where
  pretty = para alg
    where
    alg :: Base (Subgoal op term) (Subgoal op term, Doc) -> Doc
    alg (SAtomF _ atom) = pretty atom
    alg (SNullOpF _ op) = pretty op
    alg (SUnOpF _ op (ch,doc)) =
      pretty op <> mParens (SomeOp op) (operation ch) doc
    alg (SBinOpF _ op (ch,doc) (ch',doc')) =
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
  pretty AtomicFormula{..} = pretty _predSym <> parens (csep . prettyC $ _terms)

instance Pretty Term where
  pretty TVar{ _var = v } = pretty v
  pretty TSym{ _sym = s } = pretty s
  pretty TWild{}          = "_"

instance Pretty TermType where
  pretty TTInt  = "int"
  pretty TTBool = "bool"
  pretty TTText = "text"

instance Pretty Var where
  pretty (Var _ v) = pretty v

instance Pretty Sym where
  pretty (SymInt  _ i)     = int i
  pretty (SymText _ bs)    = doubleQuotes $ pretty bs
  pretty (SymBool _ True)  = "true"
  pretty (SymBool _ False) = "false"
