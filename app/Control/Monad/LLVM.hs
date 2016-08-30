module Control.Monad.LLVM
  ( L.File (..)
  , runL
  , context
  , moduleFromAst
  , moduleFromLLVMAssembly
  , hostTargetMachine
  , getTargetMachineDataLayout
  , getTargetLibraryInfo
  , getDefaultTargetTriple
  , linkModules
  , runPasses
  , writeTargetAssemblyToFile
  , writeLLVMAssemblyToFile
  )
where

import           Control.Monad.Cont
import           Control.Monad.Except

import qualified LLVM.General.AST            as AST
import qualified LLVM.General.AST.DataLayout as L
import qualified LLVM.General.Context        as L
import qualified LLVM.General.Module         as L
import qualified LLVM.General.PassManager    as L
import qualified LLVM.General.Target         as L

type Lr r e m a = ((a -> m (Either e r)) -> ExceptT e m (Either e r))

type L r m a = ContT (Either String r) m a

runL :: (Monad m) => L r m r -> m (Either String r)
runL l = runContT l (return . return)

lr2l :: (Functor m) => Lr r String m a -> L r m a
lr2l func = ContT (fmap join . runExceptT . func)

{-
l2lr :: (Functor m) => L r m a -> Lr r String m a
l2lr l = ExceptT . fmap return . runContT l
-}

liftET :: (Monad m) => ExceptT String m a -> L r m a
liftET e = ContT (\f -> runExceptT e >>= either (return . Left) f)

context :: L r IO L.Context
context = lr2l $ lift . L.withContext

moduleFromAst :: L.Context -> AST.Module -> L r IO L.Module
moduleFromAst c m = lr2l $ L.withModuleFromAST c m

moduleFromLLVMAssembly :: L.Context -> L.File -> L r IO L.Module
moduleFromLLVMAssembly c f = lr2l $ ee2e . L.withModuleFromLLVMAssembly c f
  where
    ee2e = withExceptT (either id show)

hostTargetMachine :: L r IO L.TargetMachine
hostTargetMachine = lr2l L.withHostTargetMachine

getTargetMachineDataLayout :: L.TargetMachine -> L r IO L.DataLayout
getTargetMachineDataLayout tm = lift $ L.getTargetMachineDataLayout tm

getTargetLibraryInfo :: String -> L r IO L.TargetLibraryInfo
getTargetLibraryInfo tt = lr2l $ lift . L.withTargetLibraryInfo tt

getDefaultTargetTriple :: L r IO String
getDefaultTargetTriple = lift L.getDefaultTargetTriple

linkModules :: Bool -> L.Module -> L.Module -> L () IO ()
linkModules preserveRight m m' = liftET $ L.linkModules preserveRight m m'

runPasses :: L.PassSetSpec -> L.Module -> L () IO Bool
runPasses pss m = lift $ L.withPassManager pss $ \pm -> L.runPassManager pm m

writeTargetAssemblyToFile :: L.TargetMachine -> L.File -> L.Module -> L () IO ()
writeTargetAssemblyToFile tm f m = liftET $ L.writeTargetAssemblyToFile tm f m

writeLLVMAssemblyToFile :: L.File -> L.Module -> L () IO ()
writeLLVMAssemblyToFile f m = liftET $ L.writeLLVMAssemblyToFile f m
