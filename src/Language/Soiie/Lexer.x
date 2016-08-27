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
  "while"            { tok TokWhile }
  "then"             { tok TokThen }
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
  ">"                { tok TokGT }
  "<"                { tok TokLT }
  ">="               { tok TokGE }
  "<="               { tok TokLE }
  "=="               { tok TokEQ }
  [a-z][a-z0-9]*     { tokRaw (TokVarId . unpack) }
  [0-9]              { tokRaw (TokInt . fst . fromJust . readInteger) }

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
                | TokWhile
                | TokThen
                | TokEnd
                | TokPrint
                | TokParenOpen
                | TokParenClose
                | TokComma
                | TokAssign
                | TokPlus
                | TokMinus
                | TokTimes
                | TokDivide
                | TokGT
                | TokLT
                | TokGE
                | TokLE
                | TokEQ
                | TokInt        Integer
                | TokVarId      String
  deriving (Show)
}
