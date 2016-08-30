{-# LANGUAGE DeriveFunctor #-}

module Language.Soiie.AST where

import           Data.Functor.Foldable (Fix (..))
import           Data.Sequence         (Seq)

data File = File
            { fileParams :: Seq VarId
            , fileVars   :: Seq VarId
            , fileStmts  :: Seq Stmt
            }
  deriving (Show, Eq)

newtype VarId = VarId { varIdName :: String }
  deriving (Show, Eq)

type Stmt = Fix StmtF
data StmtF a = Assign VarId Exp
             | Print  Exp
             | If     Cond (Seq a) (Seq a)
             | While  Cond (Seq a)
  deriving (Show, Eq, Functor)

sAssign :: VarId -> Exp                 -> Stmt
sPrint  :: Exp                          -> Stmt
sIf     :: Cond -> Seq Stmt -> Seq Stmt -> Stmt
sWhile  :: Cond -> Seq Stmt             -> Stmt

sAssign v e     = Fix $ Assign v e
sPrint  e       = Fix $ Print  e
sIf     c st se = Fix $ If     c st se
sWhile  c s     = Fix $ While  c s

data Cond = Cond Exp Cmp Exp
  deriving (Show, Eq)

data Cmp = CmpEQ
         | CmpNE
         | CmpLT
         | CmpLE
         | CmpGT
         | CmpGE
  deriving (Show, Eq)

type Exp = Fix ExpF
data ExpF a = Plus   a a
            | Minus  a a
            | Times  a a
            | Div    a a
            | Rem    a a
            | Neg    a
            | Int    Integer
            | VarRef VarId
  deriving (Show, Eq, Functor)

ePlus, eMinus, eTimes, eDiv, eRem :: Exp -> Exp -> Exp
eNeg    :: Exp     -> Exp
eInt    :: Integer -> Exp
eVarRef :: VarId   -> Exp

ePlus   e1 e2 = Fix $ Plus   e1 e2
eMinus  e1 e2 = Fix $ Minus  e1 e2
eTimes  e1 e2 = Fix $ Times  e1 e2
eDiv    e1 e2 = Fix $ Div    e1 e2
eRem    e1 e2 = Fix $ Rem    e1 e2
eNeg    e     = Fix $ Neg    e
eInt    i     = Fix $ Int    i
eVarRef v     = Fix $ VarRef v
