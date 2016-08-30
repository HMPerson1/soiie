module Main where

import           Control.Monad            (unless)
import           Control.Monad.Trans      (lift)
import qualified Data.ByteString.Lazy     as B
import           Data.Either.Combinators  (unlessRight)
import           System.Directory         (removeFile)
import           System.Environment       (getArgs, getProgName)
import           System.IO                (hClose, hPutStrLn, openTempFile,
                                           stderr)
import           System.Process           (callCommand)

import qualified Control.Monad.LLVM       as L
import qualified LLVM.General.PassManager as P
import qualified LLVM.General.Transforms  as T

import           Language.Soiie.Emit      (emit)
import           Language.Soiie.Lexer     (scan)
import           Language.Soiie.Parser    (parseFile)

main :: IO ()
main =
  do
    args <- getArgs
    (inFile,libFile) <- case args of
      [a]   -> return (a, "lib-rt/soiie_lib.ll")
      [a,b] -> return (a, b)
      _     -> do n <- getProgName; error ("usage: " ++ n ++ " FILE [LIB]")
    file <- B.readFile inFile

    let root = case reverse inFile of
                 'e':'i':'s':'.':r -> reverse r
                 _                 -> error ("unknown file type: " ++ inFile)
        outFile = root ++ ".o"
    (asmFile, asmH) <- openTempFile "" (root ++ ".s")
    hClose asmH

    let tokens = scan file
        ast    = parseFile tokens
        llvmIr = emit inFile ast

    res <- L.runL $ do
      c   <- L.context
      m   <- L.moduleFromAst c llvmIr
      lib <- L.moduleFromLLVMAssembly c (L.File libFile)
      tm  <- L.hostTargetMachine
      dl  <- L.getTargetMachineDataLayout tm
      tt  <- L.getDefaultTargetTriple
      li  <- L.getTargetLibraryInfo tt

      L.linkModules True m lib
      let
        ps1s = P.PassSetSpec
          { P.transforms = [T.InternalizeFunctions ["main"]]
          , P.dataLayout = Just dl
          , P.targetLibraryInfo = Just li
          , P.targetMachine = Just tm
          }
        ps2s = P.defaultCuratedPassSetSpec
          { P.optLevel = Just 3
          , P.useInlinerWithThreshold = Just 255
          , P.dataLayout = Just dl
          , P.targetLibraryInfo = Just li
          , P.targetMachine = Just tm
          }
      opt1r <- L.runPasses ps1s m
      opt2r <- L.runPasses ps2s m
      unless (opt1r && opt2r) . lift $ hPutStrLn stderr "optimising failed"
--    L.writeLLVMAssemblyToFile (L.File (root++".ll")) m
      L.writeTargetAssemblyToFile tm (L.File asmFile) m
    unlessRight res error

    callCommand ("cc " ++ asmFile ++ " -o " ++ outFile)
    removeFile asmFile
