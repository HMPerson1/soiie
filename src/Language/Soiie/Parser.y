{
module Language.Soiie.Parser
  ( parseFile
  )
where

import           Data.Sequence        (Seq, empty, singleton, (|>))

import           Language.Soiie.AST
import           Language.Soiie.Lexer (AlexPosn (..), Token (..),
                                       TokenClass (..))
}

%token
  NL            { Token _ TokNewline }
  'param'       { Token _ TokParam }
  'var'         { Token _ TokVar }
  'if'          { Token _ TokIf }
  'then'        { Token _ TokThen }
  'else'        { Token _ TokElse }
  'while'       { Token _ TokWhile }
  'do'          { Token _ TokDo }
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
  '%'           { Token _ TokRem }
  '=='          { Token _ TokEQ }
  '!='          { Token _ TokNE }
  '<'           { Token _ TokLT }
  '<='          { Token _ TokLE }
  '>'           { Token _ TokGT }
  '>='          { Token _ TokGE }
  VAR           { Token _ (TokVarId $$) }
  INT           { Token _ (TokInt $$) }

%tokentype { Token }

%name parseFile file

%left '+' '-'
%left '*' '/' '%'
%left NEG
%%

file :: { File }
        : NL file1                      { $2 }
        | file1                         { $1 }

file1 :: { File }
        : maybe_params maybe_vars stmts { File $1 $2 $3 }

maybe_params :: { Seq VarId }
        : 'param' vardecls NL           { $2 }
        | {- empty -}                   { empty }

maybe_vars :: { Seq VarId }
        : 'var' vardecls NL             { $2 }
        | {- empty -}                   { empty }

vardecls :: { Seq VarId }
        : vardecls ',' varid            { $1 |> $3 }
        | vardecls ','                  { $1 }
        | varid                         { singleton $1 }

stmts :: { Seq Stmt }
        : stmts stmt                    { $1 |> $2 }
        | stmt                          { singleton $1 }

stmt :: { Stmt }
        : varid '=' exp NL              { sAssign $1 $3 }
        | 'print' exp NL                { sPrint $2 }
        | if_stmt                       { $1 }
        | 'while' cond d_block          { sWhile $2 $3 }

if_stmt :: { Stmt }
        : 'if' cond t_block e_block     { sIf $2 $3 $4 }
        | 'if' cond t_block 'end' NL    { sIf $2 $3 empty }

t_block :: { Seq Stmt }
        : 'then' NL stmts               { $3 }

e_block :: { Seq Stmt }
        : 'else' NL stmts 'end' NL      { $3 }
        | 'else' if_stmt                { singleton $2 }

d_block :: { Seq Stmt }
        : 'do' NL stmts 'end' NL        { $3 }

cond :: { Cond }
        : exp cmp exp                   { Cond $1 $2 $3 }

cmp :: { Cmp }
        : '=='                          { CmpEQ }
        | '!='                          { CmpNE }
        | '<'                           { CmpLT }
        | '<='                          { CmpLE }
        | '>'                           { CmpGT }
        | '>='                          { CmpGE }

exp :: { Exp }
        : exp '+' exp                   { ePlus $1 $3 }
        | exp '-' exp                   { eMinus $1 $3 }
        | exp '*' exp                   { eTimes $1 $3 }
        | exp '/' exp                   { eDiv $1 $3 }
        | exp '%' exp                   { eRem $1 $3 }
        | '-' exp %prec NEG             { eNeg $2 }
        | INT                           { eInt $1 }
        | varid                         { eVarRef $1 }
        | '(' exp ')'                   { $2 }

varid :: { VarId }
        : VAR                           { VarId $1 }

{
happyError a = error ("Parse error near " ++ (case a of (Token (AlexPn _ l c) _ :_) -> '(' : show l ++ ':' : show c ++ ")"; [] -> "end of file"))
}
