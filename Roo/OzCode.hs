-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yangm, Wenrui Zhang             --
--  Implemented by Team: GNU_project                     --
-----------------------------------------------------------

module OzCode(writeCode) where

-- writeCode :: () -> String
-- writeCode _ = "writeCode"

-- DOUBLE CHECK PLZ --wenruiz
type Register = Int

data stackInstruction
    = PushStackFrame Int
    | PopStachFrame Int
    | Store Int Register
    | Load Register Int
    | LoadAddress Register Int
    | LoadIndirect Register Register
    | StoreIndirect Register Register
    deriving (Show, Eq)

data Constant
    = IntConst Register Int
    | RealConst Register Float
    | StringConst Register String
    deriving (Show, Eq)

data OpType 
    = OpInt
    | OpReal
    deriving (Show, Eq)

data ArithmeticInstruction
    = binary Add OpType Register Register Register
    | binary Sub OpType Register Register Register
    | binary Mul OpType Register Register Register
    | binary Div OpType Register Register Register