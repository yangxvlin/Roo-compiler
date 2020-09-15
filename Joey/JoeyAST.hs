module JoeyAST where

-----------------------------------
-- Specification of an AST for Joey
-----------------------------------

type Ident = String
 
data TypeName
  = BoolType | IntType 
    deriving (Show, Eq)

data LValue 
  = LId Ident
    deriving (Show, Eq)

data BinOp 
  = Op_add | Op_sub | Op_mul 
    deriving (Show, Eq)

data Exp
  = Lval LValue
  | BoolConst Bool
  | IntConst Int
  | BinOpExp BinOp Exp Exp
  | StrConst String
    deriving (Show, Eq)

data Decl 
  = Decl TypeName Ident 
    deriving (Show, Eq)

data Stmt 
  = Assign LValue Exp
  | Read LValue
  | Write Exp
    deriving (Show, Eq)

data Program
  = Program [Decl] [Stmt]
    deriving (Show, Eq)

