{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Soiie.Emit
    ( emit
    ) where

import           Prelude                            hiding (EQ, div, exp,
                                                     length, print, rem)

import           Control.Monad                      (forM_, join, void)
import           Data.Foldable                      (toList)
import           Data.Functor.Foldable              (cata)
import           Data.Sequence                      (Seq, length)

import qualified LLVM.General.AST.Constant          as C
import qualified LLVM.General.AST.FunctionAttribute as FA
import qualified LLVM.General.AST.Global            as G
import           LLVM.General.AST.Type              (i32, i64, i8)
import qualified LLVM.General.AST.Type              as T

import           LLVM.General.AST
import           LLVM.General.AST.IntegerPredicate

import           Language.Soiie.AST
import           Language.Soiie.Codegen

emit :: String -> File -> Module
emit modName f@File {..} = defaultModule
  { moduleName = modName
  , moduleDefinitions =
    GlobalDefinition (emitMain f) : moduleHeaders (fromIntegral (length fileParams))
  }

emitMain :: File -> Global
emitMain file = functionDefaults
  { G.name = Name "main"
  , G.parameters =
    ( [ Parameter i32                (Name "argc") []
      , Parameter (ptrTo (ptrTo i8)) (Name "argv") []
      ]
    , False
    )
  , G.functionAttributes = [Right FA.NoUnwind, Right FA.UWTable]
  , G.returnType = i32
  , G.basicBlocks = emitMainBody file
  }

emitMainBody :: File -> [BasicBlock]
emitMainBody File {..} = genBlocks . execCodegen $ emitHeader >> emitBody >> emitRet
  where
    emitHeader :: Codegen ()
    emitHeader =
      do
        paramArr <- alloca (ArrayType (fromIntegral (length fileParams)) i64)
        paPtr    <- getelemptr i64 paramArr 0
        _        <- call T.void get_params [ locRef i32 (Name "argc")
                                           , locRef (ptrTo (ptrTo i8)) (Name "argv")
                                           , paPtr
                                           ]
        forM_ (zip [0..] (toList fileParams)) $ \(i, VarId param) -> do
          paramPtr <- allocaVar param
          valPtr   <- getelemptr i64 paramArr i
          val      <- load valPtr
          store paramPtr val
        forM_ fileVars $ \(VarId var) -> do
          varPtr <- allocaVar var
          store varPtr (iconst 0)

    emitBody :: Codegen ()
    emitBody = mapM_ emitStmt fileStmts

    emitRet :: Codegen ()
    emitRet = setTerm (Do (Ret (Just (ConstantOperand (C.Int 32 0))) []))

emitStmt :: Stmt -> Codegen ()
emitStmt = cata $ \case
  Assign (VarId var) exp    -> join (store <$> lookupVar var <*> emitExp exp)
  Print  exp                -> join (void . call T.void print <$> mapM emitExp [exp])
  If     cond tStmts eStmts ->
    do
      condBlkNm <- uniqueName (Just "if.cond")
      thenBlkNm <- uniqueName (Just "if.then")
      elseBlkNm <- uniqueName (Just "if.else")
      endBlkNm  <- uniqueName (Just "if.end")
      emitCondBlk condBlkNm cond thenBlkNm elseBlkNm
      emitNewBlock thenBlkNm tStmts endBlkNm
      emitNewBlock elseBlkNm eStmts endBlkNm
      addNewBlock endBlkNm
  While  cond stmts         ->
    do
      condBlkNm <- uniqueName (Just "while.cond")
      doBlkNm   <- uniqueName (Just "while.do")
      endBlkNm  <- uniqueName (Just "while.end")
      emitCondBlk condBlkNm cond doBlkNm endBlkNm
      emitNewBlock doBlkNm stmts endBlkNm
      addNewBlock endBlkNm

emitCondBlk :: Name -> Cond -> Name -> Name -> Codegen ()
emitCondBlk condBlkNm cond trueBlkNm falseBlkNm =
  do
    setTerm (Do (Br condBlkNm []))
    addNewBlock condBlkNm
    condOp <- emitCond cond
    setTerm (Do (CondBr condOp trueBlkNm falseBlkNm []))

emitNewBlock :: Name -> Seq (Codegen ()) -> Name -> Codegen ()
emitNewBlock blockNm stmts nextBlkNm =
  do
    addNewBlock blockNm
    sequence_ stmts
    setTerm (Do (Br nextBlkNm []))

emitCond :: Cond -> Codegen Operand
emitCond (Cond e1 cmp e2) = join (icmp ip <$> emitExp e1 <*> emitExp e2)
  where
    ip :: IntegerPredicate
    ip = case cmp of
      CmpEQ -> EQ
      CmpNE -> NE
      CmpLT -> SLT
      CmpLE -> SLE
      CmpGT -> SGT
      CmpGE -> SGE

emitExp :: Exp -> Codegen Operand
emitExp = cata $ \case
  Plus   e1 e2       -> join (add <$> e1 <*> e2)
  Minus  e1 e2       -> join (sub <$> e1 <*> e2)
  Times  e1 e2       -> join (mul <$> e1 <*> e2)
  Div    e1 e2       -> join (div <$> e1 <*> e2)
  Rem    e1 e2       -> join (rem <$> e1 <*> e2)
  Neg    e           -> join (neg <$> e)
  Int    i           -> return (iconst i)
  VarRef (VarId var) -> join (load <$> lookupVar var)

--------------------------------------------------------------------------------
-- Runtime Library
--------------------------------------------------------------------------------

get_params :: Operand
get_params = gblRef (FunctionType VoidType [i32, ptrTo (ptrTo i8), ptrTo i64] False)
                    (Name "get_params")
print :: Operand
print = gblRef (FunctionType VoidType [i64] False) (Name "print")

moduleHeaders :: Integer -> [Definition]
moduleHeaders paramCount =
  [ GlobalDefinition globalVariableDefaults
    { G.name = Name "PARAM_COUNT"
    , G.isConstant = True
    , G.type' = i32
    , G.initializer = Just (C.Int 32 paramCount)
    }
  , GlobalDefinition functionDefaults
    { G.name = Name "get_params"
    , G.parameters =
      ( [ Parameter i32 (Name "argc") []
        , Parameter (ptrTo (ptrTo i8)) (Name "argv") []
        , Parameter (ptrTo i64) (Name "params") []
        ]
      , False
      )
    , G.returnType = T.void
    }
  , GlobalDefinition functionDefaults
    { G.name = Name "print"
    , G.parameters =
      ( [ Parameter i64 (Name "x") []
        ]
      , False
      )
    , G.returnType = T.void
    }
  ]
