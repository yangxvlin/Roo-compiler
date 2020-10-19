-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yangm, Wenrui Zhang             --
--  Implemented by Team: GNU_project                     --
-----------------------------------------------------------

module OzCode(writeCode) where

writeCode :: () -> String
writeCode _ = "writeCode"

type Register = Int

-- data stackInstruction
--     = PushStackFrame Int
--     | PopStachFrame Int
--     | Store Int Register
--     | Load Register Int
--     | LoadAddress Register Int
--     | LoadIndirect Register Register
--     | StoreIndirect Register Register
--     deriving (Show, Eq)

-- data ConstantInstruction
--     = IntConst Register Int
--     | RealConst Register Float
--     | StringConst Register String
--     deriving (Show, Eq)

-- data OpType 
--     = OpInt
--     | OpReal
--     deriving (Show, Eq)

-- data ArithmeticInstruction
--     = binary Add OpType Register Register Register
--     | AddOff Register Register Register
--     | binary Sub OpType Register Register Register
--     | SubOff Register Register Register
--     | binary Mul OpType Register Register Register
--     | binary Div OpType Register Register Register
--     | prefix Neg OpType Register Register
--     deriving (Show, Eq)

-- data ComparisonInstruction
--     = binary Eq OpType Register Register Register
--     | binary Ne OpType Register Register Register
--     | binary Gt OpType Register Register Register
--     | binary Ge OpType Register Register Register
--     | binary Lt OpType Register Register Register
--     | binary Le OpType Register Register Register
--     deriving (Show, Eq)

-- data LogicInstruction
--     = LogicAnd Register Register Register
--     | LogicOr Register Register Register
--     | LogicNot Register Register
--     deriving (Show, Eq)

-- data OperationInstruction
--     = ArithmeticInstruction
--     | ComparisonInstruction
--     | LogicInstruction
--     | Int2real Register Register
--     | Move Register Register
--       deriving (Show, Eq)

-- data BranchInstruction
--     = Cond Bool Register String
--     | Uncond String
--     deriving (Show, Eq)

-- data ProcedureInstruction
--     = Call String
--     | CallBuiltIn String
--     | Return
--     | Halt
--     deriving (Show, Eq)

-- data DebugInstruction
--     = DebugReg Register
--     | DebugSlot Int
--     | DebugStack
--     deriving (Show, Eq)

-- data Instruction
--     = stackInstruction
--     | ConstantInstruction
--     | OperationInstruction
--     | BranchInstruction
--     | ProcedureInstruction
--     | DebugInstruction
--     | Comment String
--     | Label String
--     deriving (Show, Eq)
