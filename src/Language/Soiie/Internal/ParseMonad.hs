{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Soiie.Internal.ParseMonad where

import           Control.Arrow        ((&&&))
import           Data.Bits            (shiftL, shiftR)
import           Data.ByteString.Lazy (ByteString)
import           Data.Int             (Int64)

import           Control.Monad.Except
import           Control.Monad.State

newtype P a = P (StateT ParseState (Except ParseError) a)
  deriving (Functor, Applicative, Monad, MonadError ParseError, MonadState ParseState)

evalP :: P a -> ParseState -> Either ParseError a
evalP (P p) st = runExcept (evalStateT p st)

runParser :: P a -> String -> ByteString -> Either ParseError a
runParser p name contents = evalP p (ParseState (AI (SrcLoc name 1 1) '\n' contents 0))

data ParseError = ParseError String
  deriving (Show)

data ParseState = ParseState
  { input :: !AlexInput
  } deriving (Show)

data AlexInput = AI
  { loc      :: !SrcLoc
  , prevChar :: !Char
  , buf      :: !ByteString
  , pos      :: !Int64
  } deriving (Show)

data SrcLoc = SrcLoc
  { file :: !String
  , line :: !Int
  , col  :: !Int
  } deriving (Show)

lexFail :: P a
lexFail =
  do
    (l,c) <- gets ((line &&& col) . loc . input)
    throwError (ParseError ("Lexical error at " ++ "(" ++ show l ++ ":" ++ show c ++ ")"))

parseFail :: P a
parseFail =
  do
    (l,c) <- gets ((line &&& col) . loc . input)
    throwError (ParseError ("Parse error near " ++ "(" ++ show l ++ ":" ++ show c ++ ")"))

-- copied from ghc (compiler/basicTypes/SrcLoc.hs)
-- note that this defines the tab size to be 8
advanceSrcLoc :: SrcLoc -> Char -> SrcLoc
advanceSrcLoc (SrcLoc f l _) '\n' = SrcLoc f (l + 1) 1
advanceSrcLoc (SrcLoc f l c) '\t' = SrcLoc f l (((((c-1) `shiftR` 3) + 1) `shiftL` 3) + 1)
advanceSrcLoc (SrcLoc f l c) _    = SrcLoc f l (c + 1)

data Token = Token SrcLoc TokenClass
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
                | TokEof
  deriving (Show)
