{
module Language.Vanillalog.Parser.Parser where

import Prelude hiding (lex, span)
import Protolude (Text, bimap, pure)

import           Language.Vanillalog.AST
import qualified Language.Vanillalog.Generic.AST as G
import qualified Language.Vanillalog.Generic.Parser.Lexeme as L
import           Language.Vanillalog.Generic.Parser.SrcLoc
import           Language.Vanillalog.Parser.Lexer (Token(..), lex)
}

%name programParser1 PROGRAM
%name clauseFactParser1 CLAUSE
%tokentype { L.Lexeme (Token Text) }
%error     { parseError }

%token
  "("      { L.Lexeme{L._token = TLeftPar} }
  ")"      { L.Lexeme{L._token = TRightPar} }
  "."      { L.Lexeme{L._token = TDot} }
  ","      { L.Lexeme{L._token = TComma} }
  ";"      { L.Lexeme{L._token = TSemicolon} }
  ":-"     { L.Lexeme{L._token = TRule} }
  "?-"     { L.Lexeme{L._token = TQuery} }
  "!"      { L.Lexeme{L._token = TNeg} }

  id       { L.Lexeme{L._token = TID{}} }
  str      { L.Lexeme{L._token = TStr{}} }
  int      { L.Lexeme{L._token = TInt{}} }
  bool     { L.Lexeme{L._token = TBool{}} }
  eof      { L.Lexeme{L._token = TEOF} }

%left ";"
%left ","
%left "!"

%%

PROGRAM :: { Program }
: CLAUSES eof { G.Program (span $1) . reverse $ $1 }

CLAUSES :: { [ Sentence ] }
: CLAUSES CLAUSE { $2 : $1 }
|                { [] }

CLAUSE :: { Sentence }
: ATOMIC_FORMULA ":-" SUBGOAL "." { let s = span ($1,$2) in G.SClause s $ G.Clause s $1 $3 }
| ATOMIC_FORMULA "."              { let s = span ($1,$2) in G.SFact   s $ G.Fact   s $1 }
| "?-" SUBGOAL "."                { let s = span ($1,$3) in G.SQuery  s $ G.Query  s Nothing $2 }

SUBGOAL :: { Subgoal }
: ATOMIC_FORMULA      { SAtom (span $1) $1 }
| "!" SUBGOAL         { SNeg (span ($1,$2)) $2 }
| "(" SUBGOAL ")"     { $2 }
| SUBGOAL "," SUBGOAL { SConj (span ($1,$3)) $1 $3 }
| SUBGOAL ";" SUBGOAL { SDisj (span ($1,$3)) $1 $3 }

ATOMIC_FORMULA :: { AtomicFormula }
: ID "(" TERMS ")" { uncurry AtomicFormula $1 (reverse $3) }
| ID               { uncurry AtomicFormula $1 [] }

TERMS :: { [ Term ] }
: TERMS "," TERM { $3 : $1 }
| TERM           { [ $1 ] }

TERM :: { Term }
: ID   { TVar (fst $1) (Var . snd $ $1) }
| SYM  { uncurry TSym $1 }

SYM :: { (SrcSpan, Sym) }
: str  { (span $1, SymText  . _str  . L._token $ $1) }
| int  { (span $1, SymInt   . _int  . L._token $ $1) }
| bool { (span $1, SymBool . _bool . L._token $ $1) }

ID :: { (SrcSpan, Text) }
: id { (span $1, _str . L._token $ $1) }

{
parseError :: [ L.Lexeme (Token Text) ] -> a
parseError = error . show

programParser    file = fmap programParser1    <$> lex file
clauseFactParser file = fmap clauseFactParser1 <$> lex file
}
