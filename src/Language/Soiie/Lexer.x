{
module Language.Soiie.Lexer
  ( AlexPosn(..)
  , Token(..)
  , TokenClass(..)
  , scan
  )
where

import           Data.ByteString.Lazy.Char8 (ByteString, readInteger, unpack)
import           Data.Maybe                 (fromJust)
}

%wrapper "posn-bytestring"

$white_no_nl = $white # [\n]

tokens :-
  $white_no_nl+      ;
  (("--" .*)? [\n])+ { tok TokNewline }
  "param"            { tok TokParam }
  "var"              { tok TokVar }
  "if"               { tok TokIf }
  "then"             { tok TokThen }
  "else"             { tok TokElse }
  "while"            { tok TokWhile }
  "do"               { tok TokDo }
  "end"              { tok TokEnd }
  "print"            { tok TokPrint }
  ","                { tok TokComma }
  "("                { tok TokParenOpen }
  ")"                { tok TokParenClose }
  "="                { tok TokAssign }
  "+"                { tok TokPlus }
  "-"                { tok TokMinus }
  "*"                { tok TokTimes }
  "/"                { tok TokDivide }
  "%"                { tok TokRem }
  "=="               { tok TokEQ }
  "!="               { tok TokNE }
  "<"                { tok TokLT }
  "<="               { tok TokLE }
  ">"                { tok TokGT }
  ">="               { tok TokGE }
  [a-z][a-z0-9]*     { tokRaw (TokVarId . unpack) }
  [0-9]+             { tokRaw (TokInt . fst . fromJust . readInteger) }

{
tokRaw :: (ByteString -> TokenClass) -> AlexPosn -> ByteString -> Token
tokRaw f pos bs = Token pos (f bs)
tok = tokRaw . const

scan :: ByteString -> [Token]
scan = alexScanTokens

data Token = Token AlexPosn TokenClass
  deriving (Show)

data TokenClass = TokNewline
                | TokParam
                | TokVar
                | TokIf
                | TokThen
                | TokElse
                | TokWhile
                | TokDo
                | TokEnd
                | TokPrint
                | TokComma
                | TokParenOpen
                | TokParenClose
                | TokAssign
                | TokPlus
                | TokMinus
                | TokTimes
                | TokDivide
                | TokRem
                | TokEQ
                | TokNE
                | TokLT
                | TokLE
                | TokGT
                | TokGE
                | TokInt        Integer
                | TokVarId      String
  deriving (Show)
}
