module Language.Soiie.Parser
    ( ParseError (..)
    , parseFile
    ) where

import           Data.ByteString.Lazy               (ByteString)

import           Language.Soiie.AST
import           Language.Soiie.Internal.ParseMonad
import           Language.Soiie.Internal.Parser

parseFile :: String -> ByteString -> Either ParseError File
parseFile = runParser parseFileP
