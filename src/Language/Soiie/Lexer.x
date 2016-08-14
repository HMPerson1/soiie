{
module Language.Soiie.Lexer
  ( AlexPosn(..)
  , Token(..)
  , TokenClass(..)
  , scan
  )
where

import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Lazy       as B

import Data.Maybe (fromJust)
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
  [a-z][a-z0-9]*     { tokRaw (TokVarId . BC.unpack) }
  [0-9]              { tokRaw (TokInt . fst . fromJust . BC.readInt) }

{
tokRaw :: (B.ByteString -> TokenClass) -> AlexPosn -> B.ByteString -> Token
tokRaw f pos bs = Token pos (f bs)
tok = tokRaw . const

scan :: B.ByteString -> [Token]
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
                | TokInt        Int
                | TokVarId      String
  deriving (Show)
}
