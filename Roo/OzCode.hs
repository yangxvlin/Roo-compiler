-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yang, Wenrui Zhang              --
--  Implemented by Team: GNU_project                     --
-----------------------------------------------------------

module OzCode where

type Register = Int

data StackInstruction
    = PushStackFrame Int
    | PopStackFrame Int
    | Store Int Register
    | Load Register Int
    | LoadAddress Register Int
    | LoadIndirect Register Register
    | StoreIndirect Register Register
    deriving (Eq)

data ConstantInstruction
    = OzIntConst Register Int
    | OzRealConst Register Float
    | OzStringConst Register String
    deriving (Eq)

data OpType 
    = OpInt
    | OpReal
    deriving (Eq)

data ArithmeticInstruction
    = Add OpType Register Register Register
    | AddOff Register Register Register
    | Sub OpType Register Register Register
    | SubOff Register Register Register
    | Mul OpType Register Register Register
    | Div OpType Register Register Register
    | Neg OpType Register Register
    deriving (Eq)

data ComparisonOperator
    = Eq | Ne | Gt | Ge | Lt | Le
    deriving (Eq)

data ComparisonInstruction
    = CmpInstruction ComparisonOperator OpType Register Register Register
    deriving (Eq)

data LogicInstruction
    = LogicAnd Register Register Register
    | LogicOr Register Register Register
    | LogicNot Register Register
    deriving (Eq)

data OperationInstruction
    = IntToReal Register Register
    | Move Register Register
    deriving (Eq)

data BranchInstruction
    = Cond Bool Register String
    | Uncond String
    deriving (Eq)

data ProcedureInstruction
    = ICall String
    | ICallBuiltIn String
    | IReturn
    | IHalt
    deriving (Eq)

data DebugInstruction
    = DebugReg Register
    | DebugSlot Int
    | DebugStack
    deriving (Eq)

data OzInstruction
    = StackInstruction StackInstruction
    | ArithmeticInstruction ArithmeticInstruction
    | ComparisonInstruction ComparisonInstruction
    | LogicInstruction LogicInstruction
    | ConstantInstruction ConstantInstruction
    | OperationInstruction OperationInstruction
    | BranchInstruction BranchInstruction
    | ProcedureInstruction ProcedureInstruction
    | DebugInstruction DebugInstruction
    | Comment String
    | Label String
    deriving (Eq)

    
instance Show StackInstruction where
    show (PushStackFrame size) = "push_stack_frame " ++ show (size)
    show (PopStackFrame size) = "pop_stack_frame " ++ show (size)
    show (Store slotnum register) = "store " ++ show (slotnum)
        ++ " " ++ show (register)
    show (Load register slotnum) = "load " ++ show (register) ++ " "
        ++ show (slotnum)
    show (LoadAddress register slotnum) = "load_address " 
        ++ show (register) ++ " "++ show (slotnum)
    show (LoadIndirect register1 register2) = "load_indirect " 
        ++ show (register1) ++ " " ++ show (register2)
    show (StoreIndirect register1 register2) = "store_indirect " 
        ++ show (register1) ++ " " ++ show (register2)

instance Show ConstantInstruction where
    show (OzIntConst register int) = "int_const " ++ show (register)
        ++ " " ++ show (int)
    show (OzRealConst register float) = "real_const " ++ show (register)
        ++ " " ++ show (float)
    show (OzStringConst register string) = "string_const " ++ show (register)
        ++ " " ++ show (string)

instance Show OpType where
    show OpInt = "int"
    show OpReal = "real"

instance Show ArithmeticInstruction where
    show (Add opType r1 r2 r3) = "add_" ++ show (opType) ++ " "
        ++ show (r1) ++ " " ++ show (r2) ++ " " ++ show (r3)
    show (AddOff r1 r2 r3) = "add_offset "
        ++ show (r1) ++ " " ++ show (r2) ++ " " ++ show (r3)
    show (Sub opType r1 r2 r3) = "sub_" ++ show (opType) ++ " "
        ++ show (r1) ++ " " ++ show (r2) ++ " " ++ show (r3)
    show (SubOff r1 r2 r3) = "sub_offset "
        ++ show (r1) ++ " " ++ show (r2) ++ " " ++ show (r3)
    show (Mul opType r1 r2 r3) = "mul_" ++ show (opType) ++ " "
        ++ show (r1) ++ " " ++ show (r2) ++ " " ++ show (r3)
    show (Div opType r1 r2 r3) = "div_" ++ show (opType) ++ " "
        ++ show (r1) ++ " " ++ show (r2) ++ " " ++ show (r3)
    show (Neg opType r1 r2) = "neg_" ++ show (opType) ++ " "
        ++ show (r1) ++ " " ++ show (r2)

instance Show ComparisonOperator where
    show Eq = "cmp_eq"
    show Ne = "cmp_ne"
    show Gt = "cmp_gt"
    show Ge = "cmp_ge"
    show Lt = "cmp_lt"
    show Le = "cmp_le"

instance Show ComparisonInstruction where
    show (CmpInstruction comparisonOp opType r1 r2 r3) = 
        show (comparisonOp) ++ show (opType) ++ " " 
        ++ show (r1) ++ " " ++ show (r2) ++ " " ++ show (r3)

instance Show LogicInstruction where
    show (LogicAnd r1 r2 r3) = "and " ++ show (r1) 
        ++ " " ++ show (r2) ++ " " ++ show (r3)
    show (LogicOr r1 r2 r3) = "or " ++ show (r1) 
        ++ " " ++ show (r2) ++ " " ++ show (r3)
    show (LogicNot r1 r2) = "not " ++ show (r1) ++ " " ++ show (r2)

instance Show OperationInstruction where
    show (IntToReal r1 r2) = "int_to_real " ++ show (r1) ++ " " ++ show (r2)
    show (Move r1 r2) = "move " ++ show (r1) ++ " " ++ show (r2)

instance Show BranchInstruction where
    show (Cond bool register label) = "branch_on_" ++ show (bool) 
        ++ " " ++ show (register) ++ " " ++ show (label)
    show (Uncond label) = "branch_uncond " ++ show (label)

instance Show ProcedureInstruction where
    show (ICall label) = "call " ++ show (label) 
    show (ICallBuiltIn label) = "call_builtin " ++ show (label) 
    show (IReturn) = "return"
    show (IHalt) = "halt"

instance Show DebugInstruction where
    show (DebugReg register) = "debug_reg " ++ show (register) 
    show (DebugSlot slotNum) = "debug_slot " ++ show (slotNum) 
    show (DebugStack) = "debug_stack"

instance Show OzInstruction where
    show (StackInstruction instruction) = "    " ++ show (instruction)
    show (ConstantInstruction instruction) = "    " ++ show (instruction)
    show (OperationInstruction instruction) = "    " ++ show (instruction)
    show (BranchInstruction instruction) = "    " ++ show (instruction)
    show (ProcedureInstruction instruction) = "    " ++ show (instruction)
    show (DebugInstruction instruction) = "    " ++ show (instruction)
    show (Comment comment) = "# " ++ show (comment)
    show (Label label) = show (label) ++ ";"

writeCode :: [OzInstruction] -> String
writeCode instructions = concat $ map (\x -> (show x) ++ "\n") instructions
