-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yang, Wenrui Zhang              --
--  Implemented by Team: GNU_project                     --
-----------------------------------------------------------

module OzCode where

writeCode :: () -> String
writeCode _ = "writeCode"

type Register = Int

data StackInstruction
    = PushStackFrame Int
    | PopStachFrame Int
    | Store Int Register
    | Load Register Int
    | LoadAddress Register Int
    | LoadIndirect Register Register
    | StoreIndirect Register Register
    deriving (Show, Eq)

data ConstantInstruction
    = OzIntConst Register Int
    | OzRealConst Register Float
    | OzStringConst Register String
    deriving (Show, Eq)

data OpType 
    = OpInt
    | OpReal
    deriving (Show, Eq)

data ArithmeticInstruction
    = Add OpType Register Register Register
    | AddOff Register Register Register
    | Sub OpType Register Register Register
    | SubOff Register Register Register
    | Mul OpType Register Register Register
    | Div OpType Register Register Register
    | Neg OpType Register Register
    deriving (Show, Eq)

data ComparisonInstruction
    = Eq OpType Register Register Register
    | Ne OpType Register Register Register
    | Gt OpType Register Register Register
    | Ge OpType Register Register Register
    | Lt OpType Register Register Register
    | Le OpType Register Register Register
    deriving (Show, Eq)

data LogicInstruction
    = LogicAnd Register Register Register
    | LogicOr Register Register Register
    | LogicNot Register Register
    deriving (Show, Eq)

data OperationInstruction
    = ArithmeticInstruction
    | ComparisonInstruction
    | LogicInstruction
    | Int2real Register Register
    | Move Register Register
      deriving (Show, Eq)

data BranchInstruction
    = Cond Bool Register String
    | Uncond String
    deriving (Show, Eq)

data ProcedureInstruction
    = Call String
    | CallBuiltIn String
    | Return
    | Halt
    deriving (Show, Eq)

data DebugInstruction
    = DebugReg Register
    | DebugSlot Int
    | DebugStack
    deriving (Show, Eq)

data OzInstruction
    = StackInstruction
    | ConstantInstruction
    | OperationInstruction
    | BranchInstruction
    | ProcedureInstruction
    | DebugInstruction
    | Comment String
    | Label String
    deriving (Show, Eq)
