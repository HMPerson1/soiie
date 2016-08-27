module Main where

import           Control.Monad.Except     (runExceptT)

import qualified Data.ByteString.Lazy     as B

import qualified LLVM.General.Context     as L
import qualified LLVM.General.Module      as L

import           Language.Soiie.Emit
import           Language.Soiie.Lexer
import           Language.Soiie.Parser    (parseFile)

main :: IO ()
main = do
  file <- B.getContents
  let tokens = scan file
      ast    = parseFile tokens
      llvmIr = emit ast
  out <- L.withContext $ \c ->
    runExceptT $ L.withModuleFromAST c llvmIr L.moduleLLVMAssembly
  putStrLn (either show id out)
