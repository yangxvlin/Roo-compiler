-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Joey Compiler                     --
--  Implemented by Xulin Yang                            --
-----------------------------------------------------------
module JoeyAST where

-----------------------------------
-- Specification of an AST for Joey
-----------------------------------

import Text.Parsec.Pos

-- Identifier: String
type Ident = String
 
-- Position: SourcePos (SourceName, Line, Column)
type Pos = SourcePos

-- -----------------------------------------------------------------
-- customized by Xulin Yang
-- -----------------------------------------------------------------

-- Base type: boolean, integer, string type indicator
--    for example: integer var or boolean flag or "hello world"
data BaseType
  = BooleanType
  | IntegerType
  -- | StringType  -- no sure whether we need this because 
                -- No variables can have string type, and
                -- no operations are available on strings.
                -- string literals are available, so that meaningful messages can be printed from a Roo program.
    deriving (Show, Eq)

-- A boolean literal is false or true.
type BooleanLiteral = Bool
-- An integer literal is a sequence of digits, possibly preceded by a minus sign.
type IntegerLiteral = Int
-- A string literal is a sequence of characters between double quotes.
--   The sequence itself cannot contain double quotes or newline/tab characters. 
--   It may, however, contain '" ', '\n', and '\t', respectively, to represent 
--   those characters.
type StringLiteral = String

type AliasType = String

-- for Array, VariableDecl: they have either boolean, integer, or a type alias
data DataType
  = BasyDataType BaseType
  | AliasDataType AliasType
    deriving (Show, Eq)

-- An lvalue (<lvalue>) has four (and only four) possible forms:
-- An example lvalue is point[0].xCoord
data LValue 
  = LId Ident -- <id>
  | LDot Ident Ident -- <id> . <id>
  | LBrackets Ident Exp -- <id> [ <exp> ]
  | LBracketsDot Ident Exp Ident -- <id> [ <exp> ] . <id>
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
-- data BinOp 
--   = Op_or -- or
--   | Op_and -- and
--   | Op_eq | Op_neq | Op_less | Op_less_eq | Op_large | Op_large_eq -- = != < <= > >=
--   | Op_add | Op_sub -- + -
--   | Op_mul | Op_div -- * /
--     deriving (Show, Eq)
-- data UnOp
--   = Op_not -- not; below or, above and
--   | Op_neg  -- -; below * /
--     deriving (Show, Eq)

-- -- An expression (<exp>) has one of the following forms:
-- data Exp
--   = Lval LValue -- <lvalue>
--   | BoolConst BooleanLiteral -- <const> where <const> is the syntactic category of boolean, integer, and string literals.
--   | IntConst IntegerLiteral
--   | StrConst StringLiteral
--   -- ( <exp> ) is ignored here but handelled in parser
--   | BinOpExp BinOp Exp Exp -- <exp> <binop> <exp>
--   | UnOpExp UnOp Exp -- <unop> <exp>
--     deriving (Show, Eq)

data Exp
  = Lval LValue -- <lvalue>
  | BoolConst BooleanLiteral -- <const> where <const> is the syntactic category of boolean, integer, and string literals.
  | IntConst IntegerLiteral
  | StrConst StringLiteral
  -- ( <exp> ) is ignored here but handelled in parser
  | Op_or Exp Exp
  | Op_and Exp Exp -- <exp> <binop> <exp>
  | Op_eq  Exp Exp
  | Op_neq  Exp Exp
  | Op_less  Exp Exp
  | Op_less_eq  Exp Exp
  | Op_large  Exp Exp
  | Op_large_eq  Exp Exp
  | Op_add  Exp Exp
  | Op_sub  Exp Exp
  | Op_mul  Exp Exp
  | Op_div  Exp Exp
  | Op_not Exp-- <unop> <exp>
  | Op_neg Exp -- unary -
    deriving (Show, Eq)

-- atom statement:
--     <lvalue> <- <exp> ;
--     read <lvalue> ;
--     write <exp> ;
--     writeln <exp> ;
--     call <id> ( <exp-list> ) ; 
--         where <exp-list> is a (possibly empty) comma-separated list of expressions.
-- composite statement:
--     if <expr> then <stmt-list> else <stmt-list> fi
--     if <exp> then <stmt-list> fi # just make second [Stmt] emoty
--     while <expr> do <stmt-list> od
--         where <stmt-list> is a non-empty sequence of statements, atomic or composite
data Stmt 
  -- atom statement:
  = Assign LValue Exp -- <lvalue> <- <exp>;
  | Read LValue -- read <lvalue>;
  | Write Exp -- write <exp>;
  | Writeln Exp -- writeln <exp>;
  | Call Ident [Exp] -- call <id> ( <exp-list> ); 
                     -- where <exp-list> is a (possibly empty) comma-separated list of expressions.
  -- composite statement:
  | If Exp [Stmt] [Stmt] -- if <exp> then <stmt-list> else <stmt-list> fi
                         -- if <exp> then <stmt-list> fi # just make second [Stmt] empty
  | While Exp [Stmt]     -- while <expr> do <stmt-list> od
                    -- where <stmt-list> is a non-empty sequence of statements, atomic or composite
    deriving (Show, Eq)

-- Each formal parameter has two components (in the given order):
-- 1. a parameter type/mode indicator, which is one of these five:
--   a) a type alias and an identifier,
--   b) boolean,
--   c) integer,
--   d) boolean and an identifier
--   e) integer and an identifier
data Parameter
  = DataParameter DataType Ident
  | BooleanParameter BooleanLiteral
  | IntegerParameter IntegerLiteral
    deriving (Show, Eq)

-- The header has two components (in this order):
--   1. an identifier (the procedure's name), and
--   2. a comma-separated list of zero or more formal parameters within a pair 
--      of parentheses (so the parentheses are always present).
data ProcedureHeader
  = ProcedureHeader Ident [Parameter]
    deriving (Show, Eq)

-- A variable declaration consists of
--   a) a type name (boolean, integer, or a type alias),
--   b) followed by a non-empty (enforced in parser) comma-separated list of 
  --    identifiers,
--     i)  the list terminated with a semicolon.
--     ii) There may be any number of variable declarations, in any order.
data VariableDecl
  = VariableDecl DataType [Ident]
    deriving (Show, Eq)

-- procedure body consists of 0+ local variable declarations,
-- 1. A variable declaration consists of
--   a) a type name (boolean, integer, or a type alias),
--   b) followed by a non-empty comma-separated list of identifiers,
--     i)  the list terminated with a semicolon.
--     ii) There may be any number of variable declarations, in any order.
-- 2. followed by a non-empty (enforced in parser) sequence of statements,
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
  = Array IntegerLiteral DataType Ident
    deriving (Show, Eq)

-- field declaration is of:
--   1. boolean or integer
--   2. followed by an identifier (the field name).
data FieldDecl
  = FieldDecl BaseType Ident
    deriving (Show, Eq)

-- record consists of:
--   1. the keyword record,
--   2. a non-empty (enforced in parser) list of field declarations, separated 
--      by semicolons, the whole list enclosed in braces,
--   3. an identifier, and
--   4. a semicolon.
data Record
  = Record [FieldDecl] Ident
    deriving (Show, Eq)

-- A Roo program consists of 
--   1. zero or more record type definitions, followed by 
--   2. zero or more array type definitions, followed by 
--   3. one or more (enforced in parser) procedure definitions.
data Program
  = Program [Record] [Array] [Procedure]
    deriving (Show, Eq)

