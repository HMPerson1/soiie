module Main where

import Data.ByteString.Lazy as B

import Language.Soiie.Lexer

main :: IO ()
main = B.getContents >>= (print . scan)
