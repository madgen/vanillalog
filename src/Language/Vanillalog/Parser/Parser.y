{
module Language.Vanillalog.Parser.Parser where

import Prelude hiding (lex)

import Language.Vanillalog.AST
import Language.Vanillalog.Parser.Lexer (Token(..), lex)
}

%name programParser1 PROGRAM
%name clauseFactParser1 CLAUSE
%tokentype { Token }
%error     { parseError }

%token
  "("      { TLeftPar }
  ")"      { TRightPar }
  "."      { TDot }
  ","      { TComma }
  ":-"     { TRule }
  "!"      { TNeg }

  id       { TID $$ }
  str      { TStr $$ }
  int      { TInt $$ }
  bool     { TBool $$ }
  eof      { TEOF }

%left ","
%left "!"

%%

PROGRAM :: { Program }
: CLAUSES eof { Program . reverse $ $1 }

CLAUSES :: { [ Either Clause Fact ] }
: CLAUSES CLAUSE { $2 : $1 }
|                { [] }

CLAUSE :: { Either Clause Fact }
: ATOMIC_FORMULA ":-" SUBGOAL "." { Left  $ Clause $1 $3 }
| ATOMIC_FORMULA "."              { Right $ Fact $1 }

SUBGOAL :: { Subgoal }
: ATOMIC_FORMULA      { SAtom $1 }
| "!" SUBGOAL         { SNeg $2 }
| "(" SUBGOAL ")"     { $2 }
| SUBGOAL "," SUBGOAL { SComma $1 $3 }

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
parseError :: [ Token ] -> a
parseError = error . show

programParser = fmap programParser1 <$> lex
clauseFactParser = fmap clauseFactParser1 <$> lex
}
