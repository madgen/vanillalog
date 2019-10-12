{
module Language.Vanillalog.Parser.Parser where

import Prelude hiding (lex, span)
import Protolude (Text, bimap, pure)

import Control.Monad ((>=>))

import           Language.Exalog.Core (PredicateSymbol(..))
import qualified Language.Exalog.Logger as Log
import           Language.Exalog.SrcLoc

import           Language.Vanillalog.AST
import qualified Language.Vanillalog.Generic.AST as G
import qualified Language.Vanillalog.Generic.Parser.Lexeme as L
import           Language.Vanillalog.Parser.Lexer (Token(..), lex)
}

%name      programParser1 PROGRAM
%name      replParser1 REPL_LINE
%monad     { Log.Logger }
%tokentype { L.Lexeme (Token Text) }
%error     { parseError }

%token
  "("      { L.Lexeme{L._token = TLeftPar} }
  ")"      { L.Lexeme{L._token = TRightPar} }
  "."      { L.Lexeme{L._token = TDot} }
  ","      { L.Lexeme{L._token = TComma} }
  ":-"     { L.Lexeme{L._token = TRule} }
  "?-"     { L.Lexeme{L._token = TQuery} }

  conj     { L.Lexeme{L._token = TConj} }
  disj     { L.Lexeme{L._token = TDisj} }
  neg      { L.Lexeme{L._token = TNeg} }

  fxSym    { L.Lexeme{L._token = TFxSym{}} }
  var      { L.Lexeme{L._token = TVariable{}} }
  wild     { L.Lexeme{L._token = TWildcard} }
  str      { L.Lexeme{L._token = TStr{}} }
  int      { L.Lexeme{L._token = TInt{}} }
  bool     { L.Lexeme{L._token = TBool{}} }
  eof      { L.Lexeme{L._token = TEOF} }

%left disj
%left conj
%left neg

%%

PROGRAM :: { Program }
: CLAUSES eof { G.Program (span $1) . reverse $ $1 }

REPL_LINE :: { Query }
: "?-" SUBGOAL "." eof { G.Query (span ($1,$3)) Nothing $2 }

CLAUSES :: { [ Statement ] }
: CLAUSES CLAUSE { G.StSentence $2 : $1 }
|                { [] }

CLAUSE :: { Sentence }
: ATOMIC_FORMULA ":-" SUBGOAL "." { G.SClause $ G.Clause (span ($1,$4)) (SAtom (span $1) $1) $3 }
| ATOMIC_FORMULA "."              { G.SFact   $ G.Fact   (span ($1,$2)) (SAtom (span $1) $1) }
| "?-" SUBGOAL "."                { G.SQuery  $ G.Query  (span ($1,$3)) Nothing              $2 }

SUBGOAL :: { Subgoal Op Term}
: ATOMIC_FORMULA      { SAtom (span $1) $1 }
| neg SUBGOAL         { SNeg (span ($1,$2)) $2 }
| "(" SUBGOAL ")"     { $2 }
| SUBGOAL conj SUBGOAL { SConj (span ($1,$3)) $1 $3 }
| SUBGOAL disj SUBGOAL { SDisj (span ($1,$3)) $1 $3 }

ATOMIC_FORMULA :: { AtomicFormula Term }
: FX_SYM "(" TERMS ")" { AtomicFormula (transSpan (fst $1) (span $4)) (snd $1) (reverse $3) }
| FX_SYM "("       ")" { AtomicFormula (transSpan (fst $1) (span $3)) (snd $1) [] }

FX_SYM :: { (SrcSpan, PredicateSymbol) }
: fxSym { (span $1, PredicateSymbol . _tStr . L._token $ $1) }

TERMS :: { [ Term ] }
: TERMS "," TERM { $3 : $1 }
| TERM           { [ $1 ] }

TERM :: { Term }
: VAR  { TVar $1 }
| SYM  { TSym $1 }
| wild { TWild (span $1) }

SYM :: { Sym }
: str  { SymText (span $1) . _tStr  . L._token $ $1 }
| int  { SymInt  (span $1) . _tInt  . L._token $ $1 }
| bool { SymBool (span $1) . _tBool . L._token $ $1 }

VAR :: { Var }
: var { Var (span $1) . _tStr . L._token $ $1 }

{
parseError :: [ L.Lexeme (Token Text) ] -> Log.Logger a
parseError tokens = Log.scold (Just . span . head $ tokens) ("Parse error.")

programParser  file = lex file >=> programParser1
replParser file = lex file >=> replParser1
}
