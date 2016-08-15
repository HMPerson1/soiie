module Main where

import qualified Data.ByteString.Lazy as B

import           Language.Soiie.Lexer
import           Language.Soiie.Parser (parseFile)

main :: IO ()
main = do
  file <- B.getContents
  let tokens = scan file
  print tokens
  print (parseFile tokens)
