{
module Language.Soiie.Parser
  ( parseFile
  )
where

import           Language.Soiie.AST
import           Language.Soiie.Lexer (AlexPosn(..), Token(..), TokenClass(..))
}

%token
  NL            { Token _ TokNewline }
  'param'       { Token _ TokParam }
  'var'         { Token _ TokVar}
  'if'          { Token _ TokIf }
  'while'       { Token _ TokWhile }
  'then'        { Token _ TokThen }
  'end'         { Token _ TokEnd }
  'print'       { Token _ TokPrint }
  ','           { Token _ TokComma }
  '('           { Token _ TokParenOpen }
  ')'           { Token _ TokParenClose }
  '='           { Token _ TokAssign }
  '+'           { Token _ TokPlus }
  '-'           { Token _ TokMinus }
  '*'           { Token _ TokTimes }
  '/'           { Token _ TokDivide }
  '>'           { Token _ TokGT }
  '<'           { Token _ TokLT }
  '>='          { Token _ TokGE }
  '<='          { Token _ TokLE }
  '=='          { Token _ TokEQ }
  VAR           { Token _ (TokVarId $$) }
  INT           { Token _ (TokInt $$) }

%tokentype { Token }

%name parseFile file

%left '+' '-'
%left '*' '/'
%%

file :: { File }
        : NL file1                      { $2 }
        | file1                         { $1 }

file1 :: { File }
        : maybe_params maybe_vars stmts { File $1 $2 (reverse $3) }

maybe_params :: { [VarId] }
        : 'param' vardecls NL           { reverse $2 }
        | {- empty -}                   { [] }

maybe_vars :: { [VarId] }
        : 'var' vardecls NL             { $2 }
        | {- empty -}                   { [] }

vardecls :: { [VarId] }
        : vardecls ',' varid            { $3 : $1 }
        | vardecls ','                  { $1 }
        | varid                         { [$1] }

varid :: { VarId }
        : VAR                           { VarId $1 }

stmts :: { [Stmt] }
        : stmts stmt                    { $2 : $1 }
        | stmt                          { [$1] }

stmt :: { Stmt }
        : varid '=' exp NL              { Assign $1 $3 }
        | 'print' exp NL                { Print $2 }
        | 'if' cond block               { If $2 $3 }
        | 'while' cond block            { While $2 $3 }

cond :: { Cond }
        : exp cmp exp                   { Cond $1 $2 $3 }

cmp :: { Cmp }
        : '>'                           { CmpGT }
        | '<'                           { CmpLT }
        | '>='                          { CmpGE }
        | '<='                          { CmpLE }
        | '=='                          { CmpEQ }

block :: { [Stmt] }
        : 'then' NL stmts 'end' NL      { reverse $3 }

exp :: { Exp }
        : exp '+' exp                   { Plus $1 $3}
        | exp '-' exp                   { Minus $1 $3}
        | exp '*' exp                   { Times $1 $3 }
        | exp '/' exp                   { Div $1 $3 }
        | INT                           { Int $1 }
        | varid                         { VarRef $1 }
        | '(' exp ')'                   { $2 }

{
happyError a = error ("Parse error near " ++ (case a of (Token (AlexPn _ l c) _ :_) -> '(' : show l ++ ':' : show c ++ ")"; [] -> "end of file"))
}
