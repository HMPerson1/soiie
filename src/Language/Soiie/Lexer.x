{
{-# LANGUAGE NamedFieldPuns #-}

module Language.Soiie.Lexer
  ( lexToken
  )
where

import           Prelude                    hiding (take)

import           Data.ByteString.Internal   (w2c)
import           Data.ByteString.Lazy       (ByteString, take, uncons)
import           Data.ByteString.Lazy.Char8 (readInteger, unpack)
import           Data.Maybe                 (fromJust)
import           Data.Word                  (Word8)

import           Control.Monad.State

import           Language.Soiie.ParseMonad
}

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
tokRaw :: (ByteString -> TokenClass) -> SrcLoc -> ByteString -> Token
tokRaw f loc bs = Token loc (f bs)
tok :: TokenClass -> SrcLoc -> ByteString -> Token
tok = tokRaw . const

lexToken :: P Token
lexToken =
  do
    inp1@(AI _ _ buf1 pos1) <- gets input
    case alexScan inp1 0 of
      AlexEOF -> do
        l <- gets (loc . input)
        return (Token l TokEof)
      AlexError _ -> do
        lexFail
      AlexSkip inp2 _ -> do
        modify (\s->s{input=inp2})
        lexToken
      AlexToken inp2@(AI loc2 _ _ pos2) _ act -> do
        modify (\s->s{input=inp2})
        return (act loc2 (take (pos2 - pos1) buf1))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = prevChar

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte AI {loc,buf,pos} =
  case uncons buf of
    Nothing -> Nothing
    Just (b,buf') -> let c = w2c b in Just (b, AI (advanceSrcLoc loc c) c buf' (pos+1))
}
