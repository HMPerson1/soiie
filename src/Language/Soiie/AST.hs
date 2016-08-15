module Language.Soiie.AST where

data File = File
            { fileParams :: [VarId]
            , fileVars   :: [VarId]
            , fileStmts  :: [Stmt]
            }
  deriving (Show, Eq)

newtype VarId = VarId { varIdName :: String }
  deriving (Show, Eq)

data Stmt = Assign VarId Exp
          | Print Exp
          | If Cond [Stmt]
          | While Cond [Stmt]
  deriving (Show, Eq)

data Cond = Cond Exp Cmp Exp
  deriving (Show, Eq)

data Cmp = CmpGT
         | CmpLT
         | CmpGE
         | CmpLE
         | CmpEQ
  deriving (Show, Eq)

data Exp = Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Int Int
         | VarRef VarId
  deriving (Show, Eq)
