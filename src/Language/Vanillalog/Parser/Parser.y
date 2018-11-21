{
module Language.Vanillalog.Parser.Parser where

import Prelude hiding (lex)
import Protolude (Text, bimap, pure)

import           Language.Vanillalog.AST
import qualified Language.Vanillalog.Generic.AST as G
import qualified Language.Vanillalog.Generic.Parser.Lexeme as L
import           Language.Vanillalog.Parser.Lexer (Token(..), lex)
}

%name programParser1 PROGRAM
%name clauseFactParser1 CLAUSE
%tokentype { L.Lexeme (Token Text) }
%error     { parseError }

%token
  "("      { L.Lexeme{L.token = TLeftPar} }
  ")"      { L.Lexeme{L.token = TRightPar} }
  "."      { L.Lexeme{L.token = TDot} }
  ","      { L.Lexeme{L.token = TComma} }
  ";"      { L.Lexeme{L.token = TSemicolon} }
  ":-"     { L.Lexeme{L.token = TRule} }
  "?-"     { L.Lexeme{L.token = TQuery} }
  "!"      { L.Lexeme{L.token = TNeg} }

  id       { L.Lexeme{L.token = TID $$} }
  str      { L.Lexeme{L.token = TStr $$} }
  int      { L.Lexeme{L.token = TInt $$} }
  bool     { L.Lexeme{L.token = TBool $$} }
  eof      { L.Lexeme{L.token = TEOF} }

%left ";"
%left ","
%left "!"

%%

PROGRAM :: { Program }
: CLAUSES eof { G.Program . reverse $ $1 }

CLAUSES :: { [ Sentence ] }
: CLAUSES CLAUSE { $2 : $1 }
|                { [] }

CLAUSE :: { Sentence }
: ATOMIC_FORMULA ":-" SUBGOAL "." { G.SClause $ G.Clause $1 $3 }
| ATOMIC_FORMULA "."              { G.SFact   $ G.Fact $1 }
| "?-" SUBGOAL "."                { G.SQuery  $ G.Query Nothing $2 }

SUBGOAL :: { Subgoal }
: ATOMIC_FORMULA      { SAtom $1 }
| "!" SUBGOAL         { SNeg $2 }
| "(" SUBGOAL ")"     { $2 }
| SUBGOAL "," SUBGOAL { SConj $1 $3 }
| SUBGOAL ";" SUBGOAL { SDisj $1 $3 }

ATOMIC_FORMULA :: { AtomicFormula }
: id "(" TERMS ")" { AtomicFormula $1 (reverse $3) }
| id               { AtomicFormula $1 [] }

TERMS :: { [ Term ] }
: TERMS "," TERM { $3 : $1 }
| TERM           { [ $1 ] }

TERM :: { Term }
: id   { TVar $ Var $1 }
| SYM  { TSym $ $1 }

SYM :: { Sym }
: str  { SymText $1 }
| int  { SymInt  $1 }
| bool { SymBool $1 }

{
parseError :: [ L.Lexeme (Token Text) ] -> a
parseError = error . show

programParser    file = fmap programParser1    <$> lex file
clauseFactParser file = fmap clauseFactParser1 <$> lex file
}
