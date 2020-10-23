-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yang                            --
--  Implemented by Team: GNU_project                     --
-----------------------------------------------------------
module RooAST where

-----------------------------------
-- Terminology:
-- 0+: zero or more/possible empty
-- 1+: one or more/ non empty
--     both 0+, 1+ are stored in list [] but 1+ will be implemented in parser 
--      not here
-----------------------------------

-----------------------------------
-- Specification of an AST for Roo
-----------------------------------

-- Identifier: String
type Ident = String

-- Base type: boolean, integer type indicator
--     Not necessary to have string as no variable/parameter/declaration has 
--      string type
data BaseType
  = BooleanType
  | IntegerType
  | StringType  --Add Stringtype ,because exp has string type
    deriving (Show, Eq)

-- User custermized record type, stored as string
type AliasType = String

-- for Array, VariableDecl: they have either boolean, integer, or a type alias 
--  data type
--     factored out for reuse purpose
data DataType
  = BaseDataType BaseType
  | AliasDataType AliasType
    deriving (Show, Eq)

-- An lvalue (<lvalue>) has four (and only four) possible forms:
--     An example lvalue is point[0].xCoord
data LValue 
  = LId Ident                     -- <id>
  | LDot Ident Ident              -- <id>.<id>
  | LBrackets Ident Exp           -- <id>[<exp>]
  | LBracketsDot Ident Exp Ident  -- <id>[<exp>].<id>
    deriving (Show, Eq)

-- expression operators: 
--     All the operators on the same line have the same precedence, 
--         and the ones on later lines have lower precedence;
--     The six relational operators are non-associative 
--         so, for example, a = b = c is not a well-formed expression). 
--     The six remaining binary operators are left-associative.
-- -              |unary            |
-- * /            |binary and infix |left-associative
-- + -            |binary and infix |left-associative
-- = != < <= > >= |binary and infix |relational, non-associative 
-- not            |unary            |
-- and            |binary and infix |left-associative
-- or             |binary and infix |left-associative
data Exp
  = Lval LValue               -- <lvalue>
  | BoolConst Bool  -- <const> where <const> is the syntactic category of 
                    -- boolean, integer, and string literals.
  | IntConst Int
  | StrConst String
                              -- ( <exp> ) is ignored here but handelled in 
                              --  parser
  | Op_or Exp Exp             -- <exp> <binop: or> <exp>
  | Op_and Exp Exp            -- <exp> <binop: and> <exp>
  | Op_eq  Exp Exp            -- <exp> <binop: "="> <exp>
  | Op_neq  Exp Exp           -- <exp> <binop: "!="> <exp>
  | Op_less  Exp Exp          -- <exp> <binop: "<"> <exp>
  | Op_less_eq  Exp Exp       -- <exp> <binop: "<="> <exp>
  | Op_large  Exp Exp         -- <exp> <binop: ">"> <exp>
  | Op_large_eq  Exp Exp      -- <exp> <binop: ">="> <exp>
  | Op_add  Exp Exp           -- <exp> <binop: "+"> <exp>
  | Op_sub  Exp Exp           -- <exp> <binop: "-"> <exp>
  | Op_mul  Exp Exp           -- <exp> <binop: "*"> <exp>
  | Op_div  Exp Exp           -- <exp> <binop: "/"> <exp>
  | Op_not Exp                -- <unop: not> <exp>
  | Op_neg Exp                -- <unop: "-"> <exp>
    deriving (Show, Eq)

-- Stmt has following two category:
--  1) atom statement:
--      <lvalue> <- <exp> ;
--      read <lvalue> ;
--      write <exp> ;
--      writeln <exp> ;
--      call <id>(<exp-list>) ; 
--          where <exp-list> is a 0+ comma-separated list of expressions.
--  2) composite statement:
--      if <exp> then <stmt-list> else <stmt-list> fi
--      if <exp> then <stmt-list> fi # just make above second [Stmt] empty
--      while <exp> do <stmt-list> od
--          where <stmt-list> is a 1+ sequence of statements, atomic or composite
--  
-- the data structure for above grammer are given accordingly below
data Stmt 
  -- 1) atom statement:
  = Assign LValue Exp 
  | Read LValue 
  | Write Exp 
  | Writeln Exp 
  | Call Ident [Exp] 
  -- 2) composite statement:
  | IfThen Exp [Stmt]
  | IfThenElse Exp [Stmt] [Stmt]
  | While Exp [Stmt]     
    deriving (Show, Eq)

-- Each formal parameter has two components (in the given order):
--  1. a parameter type/mode indicator, which is one of these five:
--    a) a type alias,
--    b) boolean,
--    c) integer,
--    d) boolean val
--    e) integer val
--  2. an identifier
data Parameter
  = DataParameter DataType Ident -- a) b) c) above
  | BooleanVal Ident             -- d)       above
  | IntegerVal Ident             -- e)       above
    deriving (Show, Eq)

-- The header has two components (in this order):
--   1. an identifier (the procedure's name), and
--   2. a comma-separated list of 0+ formal parameters within a pair 
--        of parentheses (so the parentheses are always present).
data ProcedureHeader
  = ProcedureHeader Ident [Parameter]
    deriving (Show, Eq)

-- A variable declaration consists of
--   a) a type name (boolean, integer, or a type alias),
--   b) followed by a 1+ comma-separated list of 
--        identifiers,
--     i)  the list terminated with a semicolon.
--     ii) There may be any number of variable declarations, in any order.
data VariableDecl
  = VariableDecl DataType [Ident]
    deriving (Show, Eq)

-- procedure body consists of 0+ local variable declarations,
--   1. A variable declaration consists of
--     a) a type name (boolean, integer, or a type alias),
--     b) followed by a 1+ comma-separated list of identifiers,
--       i)  the list terminated with a semicolon.
--       ii) There may be any number of variable declarations, in any order.
--   2. followed by a 1+ sequence of statements,
data ProcedureBody
  = ProcedureBody [VariableDecl] [Stmt]
    deriving (Show, Eq)

-- Each procedure consists of (in the given order):
--   1. the keyword procedure,
--   2. a procedure header, and
--   3. a procedure body.
data Procedure
  = Procedure ProcedureHeader ProcedureBody
    deriving (Show, Eq)

-- array type definition consists of (in the given order):
--   1. the keyword array,
--   2. a (positive) integer literal enclosed in square brackets,
--   3. a type name which is either an identifier (a type alias) or one of 
--      boolean and integer,
--   4. an identifier (giving a name to the array type), and
--   5. a semicolon.
data Array
  = Array Int DataType Ident
    deriving (Show, Eq)

-- field declaration is of:
--   1. boolean or integer
--   2. followed by an identifier (the field name).
data FieldDecl
  = FieldDecl BaseType Ident
    deriving (Show, Eq)

-- record consists of:
--   1. the keyword record,
--   2. a 1+ list of field declarations, separated by semicolons, 
--        the whole list enclosed in braces,
--   3. an identifier, and
--   4. a semicolon.
data Record
  = Record [FieldDecl] Ident
    deriving (Show, Eq)

-- A Roo program consists of 
--   1. 0+ record type definitions, followed by 
--   2. 0+ array type definitions, followed by 
--   3. 1+ procedure definitions.
data Program
  = Program [Record] [Array] [Procedure]
    deriving (Show, Eq)
