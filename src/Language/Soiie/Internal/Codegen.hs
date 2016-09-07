{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Language.Soiie.Internal.Codegen where

import           Control.Lens                       (ASetter', Snoc, at,
                                                     makeLenses, use)
import           Control.Lens.Operators
import           Control.Monad.State                (MonadState, State,
                                                     execState)
import           Data.Foldable                      (toList)
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as M
import           Data.Maybe                         (fromMaybe)
import           Data.Sequence                      (Seq)
import qualified Data.Sequence                      as S

import           LLVM.General.AST.AddrSpace         (AddrSpace (..))
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant          as C
import           LLVM.General.AST.IntegerPredicate  (IntegerPredicate)
import           LLVM.General.AST.Type              (i1, i64)

import           LLVM.General.AST

--------------------------------------------------------------------------------
-- Codegen Monad
--------------------------------------------------------------------------------

data BlockgenState = BlockgenState
  { _name   :: Name
  , _instrs :: Seq (Named Instruction)
  , _term   :: Maybe (Named Terminator)
  } deriving (Show)

makeLenses ''BlockgenState

type SymbolTable = Map String Operand
type NameSupply = Map (Maybe String) Word

data CodegenState = CodegenState
  { _currentBlock :: BlockgenState
  , _blocks       :: Seq BlockgenState
  , _symbolTable  :: SymbolTable
  , _nameSupply   :: NameSupply
  } deriving (Show)

makeLenses ''CodegenState

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

emptyCodegenState :: CodegenState
emptyCodegenState = CodegenState entryBlk S.empty M.empty (M.singleton Nothing 1)
  where
    entryBlk = BlockgenState (UnName 0) S.empty Nothing

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegenState

genBlocks :: CodegenState -> [BasicBlock]
genBlocks cs = toList . fmap mkBlock $ (cs^.blocks) |> (cs^.currentBlock)
  where
    mkBlock :: BlockgenState -> BasicBlock
    mkBlock BlockgenState {..} =
      BasicBlock _name
                 (toList _instrs)
                 (fromMaybe (error ("no terminator for block: " ++ show _name)) _term)

--------------------------------------------------------------------------------
-- State Manipulation Helpers
--------------------------------------------------------------------------------

uniqueName :: Maybe String -> Codegen Name
uniqueName maybeString = mkName <$> mkId
  where
    mkId :: Codegen Word
    mkId = (nameSupply . at maybeString <<%= Just . (+1) . fromMaybe 0) <&> fromMaybe 0

    mkName :: Word -> Name
    mkName = case maybeString of
      Nothing -> UnName
      Just s  -> Name . (s ++) . ('.' :) . show

setTerm :: Named Terminator -> Codegen ()
setTerm t = currentBlock . term ?= t

addNewBlock :: Name -> Codegen ()
addNewBlock blkName =
  do
    c <- use currentBlock
    blocks |>= c
    currentBlock .= BlockgenState blkName S.empty Nothing

--------------------------------------------------------------------------------
-- Instructions
--------------------------------------------------------------------------------

instr :: Type -> Instruction -> Codegen Operand
instr ty i =
  do
    n <- uniqueName Nothing
    currentBlock . instrs |>= (n := i)
    return (locRef ty n)

add :: Operand -> Operand -> Codegen Operand
add op1 op2 = instr i64 (Add False False op1 op2 [])

sub :: Operand -> Operand -> Codegen Operand
sub op1 op2 = instr i64 (Sub False False op1 op2 [])

mul :: Operand -> Operand -> Codegen Operand
mul op1 op2 = instr i64 (Mul False False op1 op2 [])

div :: Operand -> Operand -> Codegen Operand
div op1 op2 = instr i64 (SDiv False op1 op2 [])

rem :: Operand -> Operand -> Codegen Operand
rem op1 op2 = instr i64 (SRem op1 op2 [])

neg :: Operand -> Codegen Operand
neg = sub (iconst 0)

icmp :: IntegerPredicate -> Operand -> Operand -> Codegen Operand
icmp ip op1 op2 = instr i1 (ICmp ip op1 op2 [])

alloca :: Type -> Codegen Operand
alloca ty = instr (ptrTo ty) (Alloca ty Nothing 0 [])

load :: Operand -> Codegen Operand
load ptr = instr i64 (Load False ptr Nothing 0 [])

store :: Operand -> Operand -> Codegen ()
store ptr val = currentBlock . instrs |>= Do (Store False ptr val Nothing 0 [])

getelemptr :: Type -> Operand -> Integer -> Codegen Operand
getelemptr ty arr idx = instr (ptrTo ty)
  (GetElementPtr True arr [iconst 0, iconst idx] [])

call :: Type -> Operand -> [Operand] -> Codegen Operand
call ty func args = instr ty Call
  { tailCallKind = Nothing
  , callingConvention = CC.C
  , returnAttributes = []
  , function = Right func
  , arguments = fmap (\x->(x, [])) args
  , functionAttributes = []
  , metadata = []
  }

--------------------------------------------------------------------------------
-- Symbol Table Helpers
--------------------------------------------------------------------------------

allocaVar :: String -> Codegen Operand
allocaVar var =
  do
    op <- alloca i64
    prev <- symbolTable . at var <<.= Just op
    case prev of
      Nothing -> return op
      _ -> error ("variable/parameter defined twice: " ++ var)

lookupVar :: String -> Codegen Operand
lookupVar var = fromMaybe die <$> use (symbolTable . at var)
  where
    die :: a
    die = error ("use of undeclared variable: " ++ var)

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

ptrTo :: Type -> Type
ptrTo t = PointerType t (AddrSpace 0)

locRef :: Type -> Name -> Operand
locRef = LocalReference

gblRef :: Type -> Name -> Operand
gblRef t n = ConstantOperand (C.GlobalReference t n)

iconst :: Integer -> Operand
iconst i = ConstantOperand (C.Int 64 i)

infixl 4 |>=

(|>=) :: (MonadState s m, Snoc a a e e) => ASetter' s a -> e -> m ()
l |>= e = l %= (|> e)
