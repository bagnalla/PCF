-- This grammar file is heavily based on
-- 1) The template found here https://github.com/dagit/happy-plus-alex
-- 2) The STLC Yacc grammar included with TAPL by Benjamin Pierce

{
{-# OPTIONS -w #-}
module Parser( parseProg ) where

import Lexer
import qualified Ast
import Symtab (Id(..))
import Core (info_of_term)
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
-- Without this we get a type error
%error { happyError }

%token
  arrow        { Token $$ TokenArrow }
  doublearrow  { Token $$ TokenDoubleArrow }
  bool         { Token $$ TokenBoolTy }
  nat          { Token $$ TokenNatTy }
  true         { Token $$ (TokenBool True) }
  false        { Token $$ (TokenBool False) }
  natVal       { Token _ (TokenNat _) }
  succ         { Token $$ TokenSucc }
  pred         { Token $$ TokenPred }
  iszero       { Token $$ TokenIszero }
  fix          { Token $$ TokenFix }
  if           { Token $$ TokenIf }
  then         { Token $$ TokenThen }
  else         { Token $$ TokenElse }
  eval         { Token $$ TokenEval }
  id           { Token _ (TokenId _) }
  '='          { Token $$ TokenEq }
  '\\'         { Token $$ TokenBackslash }
  '('          { Token $$ TokenLParen }
  ')'          { Token $$ TokenRParen }
  ':'          { Token $$ TokenColon }
  ';'          { Token $$ TokenSemicolon }
  -- eof          { Token $$ TokenEOF }

%left ';'
%nonassoc '='
%right arrow doublearrow
%right succ pred iszero
%left succ pred
%nonassoc iszero fix
%nonassoc true false natVal id
%left APP
%nonassoc '(' ')'
%nonassoc ':'
%%

Prog :
  Commands { Ast.Prog { Ast.pinfo_of = AlexPn 0 0 0, Ast.prog_of = $1 } }

-- Atomic types
AType :
  bool { Ast.TyBool }
  | nat { Ast.TyNat }
  | '(' Type ')' { $2 }

ArrowType :
  AType arrow ArrowType { Ast.TyArrow $1 $3 }
  | AType { $1 }

Type :
  ArrowType { $1 }

Term :
  AppTerm { $1 }
  | if Term then Term else Term { Ast.TmIf $1 $2 $4 $6 }
  | '\\' id TyBinder doublearrow Term {
    case $2 of
      Token _ (TokenId id) ->
        Ast.TmAbs $1 id $3 $5 }
  -- | Term Term %prec APP    { Ast.TmApp () $1 $2 }

AppTerm :
  ATerm { $1 }
  | AppTerm ATerm { Ast.TmApp (info_of_term $1) $1 $2 }
  | succ ATerm { Ast.TmSucc $1 $2 }
  | pred ATerm { Ast.TmPred $1 $2 }
  | iszero ATerm { Ast.TmIszero $1 $2 }
  | fix ATerm { Ast.TmFix $1 $2 }

-- Atomic terms
ATerm :
  '(' Term ')' { $2 }
  | true { Ast.TmTrue $1 }
  | false { Ast.TmFalse $1 }
  | natVal {
    case $1 of
      Token fi (TokenNat n) ->
        foldr (\_ acc -> Ast.TmSucc fi acc) (Ast.TmZero fi) [1..n] }
  | id {
    case $1 of
      Token fi (TokenId id) ->
        Ast.TmVar fi id }

Command :
  id Binder {
    case $1 of
      Token fi (TokenId id) ->
        Ast.CBind fi id $2 }
  | eval Term { Ast.CEval $1 $2 }

TyBinder :
  ':' Type { $2 }

Binder :
  '=' Term { $2 }

Commands :
  Commands ';' Commands { $1 ++ $3 }
  -- | Commands ';' { $1 }
  | Command { [$1] }
  | {- empty -} { [] }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseProg :: FilePath -> String -> Either String (Ast.Prog AlexPosn)
parseProg = runAlex' parse
}
