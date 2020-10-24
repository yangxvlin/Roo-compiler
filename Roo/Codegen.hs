-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yangm, Wenrui Zhang             --
--  Implemented by Team: GNU_project                     --
-----------------------------------------------------------
module Codegen(ozCode, Consequence(..)) where

import OzCode
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import RooAST
import SymbolTable
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Either

type Consequence = Either String [OzInstruction]

data Btype = Int | Bool | String

ozCode :: SymTable -> Program -> Consequence
ozCode st prog = evalStateT (codeGeneration prog) st

codeGeneration :: Program -> SymTableState [OzInstruction]
codeGeneration (Program _ _ procedures)
    = 
        do
            let generatedCode = [ProcedureInstruction $ ICall "proc_main", 
                                ProcedureInstruction $ IHalt]

            mapM_ appendInstruction generatedCode
            mapM_ generateProcedure procedures

            st <- get
            return $ instructions st

generateProcedure :: Procedure -> SymTableState ()
generateProcedure p@(Procedure (ProcedureHeader procID params) 
                                (ProcedureBody _ stmts))
    =
        do
            appendInstruction (Label $ "proc_" ++ procID)
            pushLocalVariableTable
            insertProcedureVariable p

            -- generate code of procedure's statements
            appendInstruction (Comment "prologue")
            slotNum <- getSlotCounter
            if slotNum /= 0
            then do
                appendInstruction (StackInstruction $ PushStackFrame slotNum)
            else return ()

            -- load parameters
            let paraNum = length params
            appendInstruction (Comment "load parameters")
            mapM_ (\i ->
                    do 
                        appendInstruction(StackInstruction $ Store i i)
                    ) [0..(paraNum -1)]
            
            -- init variables
            reg_init <- getRegisterCounter
            appendInstruction (Comment "initialise variables")
            appendInstruction (ConstantInstruction $ OzIntConst reg_init 0)
            mapM_ (\i ->
                    do
                        appendInstruction (StackInstruction $ Store i reg_init)
                    ) [paraNum..(slotNum -1)]
            setRegisterCounter reg_init

            mapM_ generateStatement stmts

            -- end of the procedure
            appendInstruction (Comment "epilogue")
            if slotNum /= 0
            then do
                appendInstruction (StackInstruction $ PopStackFrame slotNum)
            else return ()

            appendInstruction (ProcedureInstruction IReturn)

            popLocalVariableTable

-- ---------------------------------------------------------------------------
-- Generate Statement
-- ---------------------------------------------------------------------------            

generateStatement :: Stmt -> SymTableState ()
generateStatement (Assign lValue exp)
    = 
        do
            appendInstruction (Comment $ show lValue ++ " <- " ++ show exp)
            reg <- getRegisterCounter
            loadExp reg exp
            storeVal reg lValue
            setRegisterCounter reg

generateStatement (Read lValue)
    = 
        do
            appendInstruction (Comment $ "Read " ++ show lValue)
            bType <- getType $ Lval lValue
            reg <- getRegisterCounter 
            let cmd = case bType of Int -> "read_int"
                                    Bool -> "read_bool"
                                    String -> "read_string"
            appendInstruction (ProcedureInstruction $ ICallBuiltIn cmd)
            storeVal reg lValue
            setRegisterCounter reg
            

generateStatement (Write exp)
    = 
        do
            appendInstruction (Comment $ "Write " ++ show exp)
            bType <- getType exp
            reg <- getRegisterCounter
            loadExp reg exp
            let cmd = case bType of Int -> "print_int"
                                    Bool -> "print_bool"
                                    String -> "print_string"
            appendInstruction (ProcedureInstruction $ ICallBuiltIn cmd)
            setRegisterCounter reg

generateStatement (Writeln exp)   
    = 
        do 
            appendInstruction (Comment $ "Writeln " ++ show exp)
            bType <- getType exp
            reg <- getRegisterCounter
            loadExp reg exp
            let cmd = case bType of Int -> "print_int"
                                    Bool -> "print_bool"
                                    String -> "print_string"
            appendInstruction (ProcedureInstruction $ ICallBuiltIn cmd)
            appendInstruction (ProcedureInstruction $ ICallBuiltIn "print_newline")
            setRegisterCounter reg

generateStatement (Call procID params)
    = 
        do 
            appendInstruction (Comment $ "Call " ++ show procID)
            let paraNum = length params 
            (formalParams, _) <- getProcedure procID
            mapM_ (\i ->
                    do
                        reg <- getRegisterCounter
                        let (byValue, _) = formalParams !! i
                        case byValue of 
                            True -> loadExp reg $ params !! i
                            False -> case (params !! i) of
                                            Lval lValue -> loadVarAddress reg lValue
                                            _ -> return ()
                    ) [0..(paraNum - 1)]
            appendInstruction (ProcedureInstruction $ ICall $ "proc_" ++ procID)
            setRegisterCounter 0

-- TODO: If then
generateStatement (IfThen exp stmts) 
    = 
        do 
            trueLabel <- getlabelCounter
            falseLabel <- getlabelCounter
            appendInstruction (Comment $ "if " ++ show(exp))

            -- guard
            reg <- getRegisterCounter
            loadExp reg exp
            appendInstruction (BranchInstruction $ Cond False reg falseLabel) 
            setRegisterCounter reg

            -- then
            appendInstruction (Label trueLabel)
            appendInstruction (Comment $ "then")
            mapM_ generateStatement stmts
            appendInstruction (Comment $ "fi")
 
            -- after if then
            appendInstruction (Label falseLabel)


-- TODO: If then else
generateStatement (IfThenElse exp stmts1 stmts2) 
    = 
        do 
            trueLabel <- getlabelCounter
            falseLabel <- getlabelCounter
            endLabel <- getlabelCounter
            appendInstruction (Comment $ "if " ++ show(exp))

            -- guard
            reg <- getRegisterCounter
            loadExp reg exp
            appendInstruction (BranchInstruction $ Cond False reg falseLabel) 
            setRegisterCounter reg

            -- then
            appendInstruction (Label trueLabel)
            appendInstruction (Comment $ "then")
            mapM_ generateStatement stmts1
            appendInstruction (BranchInstruction $ Uncond endLabel) 

            -- else
            appendInstruction (Label falseLabel)
            appendInstruction (Comment $ "else")
            mapM_ generateStatement stmts2
            appendInstruction (Comment $ "fi") 

            -- after if then else
            appendInstruction (Label endLabel)

-- TODO: While
generateStatement (While exp stmts) 
    = 
        do 
            trueLabel <- getlabelCounter
            falseLabel <- getlabelCounter
            appendInstruction (Comment $ "While " ++ show(exp))
            appendInstruction (Label trueLabel)

            -- guard
            reg <- getRegisterCounter
            loadExp reg exp
            appendInstruction (BranchInstruction $ Cond False reg falseLabel) 
            setRegisterCounter reg

            -- whileloop body
            appendInstruction (Comment $ "do")
            mapM_ generateStatement stmts
            appendInstruction (BranchInstruction $ Uncond trueLabel) 
            appendInstruction (Comment $ "od")

            -- after loop
            appendInstruction (Label falseLabel)


-- load an expression to the given register
loadExp :: Int -> Exp -> SymTableState ()
loadExp reg (Lval lValue) = loadVal reg lValue
loadExp reg (BoolConst vl) 
    = appendInstruction (ConstantInstruction $ OzIntConst reg $ boolToInt vl)
loadExp reg (IntConst vl)
    = appendInstruction (ConstantInstruction $ OzIntConst reg vl)
loadExp reg (StrConst vl)
    = appendInstruction (ConstantInstruction $ OzStringConst reg vl)
-- TODO
loadExp reg (Op_or lExp rExp)
    = 
        do
            loadExp reg lExp
            reg_1 <- getRegisterCounter
            loadExp reg_1 rExp
            appendInstruction (LogicInstruction $ LogicOr reg reg reg_1) 
            setRegisterCounter reg_1 
loadExp reg (Op_and lExp rExp)
    = 
        do
            loadExp reg lExp
            reg_1 <- getRegisterCounter
            loadExp reg_1 rExp
            appendInstruction (LogicInstruction $ LogicAnd reg reg reg_1) 
            setRegisterCounter reg_1 
loadExp reg (Op_eq lExp rExp)
    = 
        do
            loadExp reg lExp
            reg_1 <- getRegisterCounter
            loadExp reg_1 rExp
            appendInstruction (ComparisonInstruction 
                $ CmpInstruction Eq OpInt reg reg reg_1)
            setRegisterCounter reg_1
loadExp reg (Op_neq lExp rExp)
    = 
        do
            loadExp reg lExp
            reg_1 <- getRegisterCounter
            loadExp reg_1 rExp
            appendInstruction (ComparisonInstruction 
                $ CmpInstruction Ne OpInt reg reg reg_1)
            setRegisterCounter reg_1
loadExp reg (Op_less lExp rExp)
    = 
        do
            loadExp reg lExp
            reg_1 <- getRegisterCounter
            loadExp reg_1 rExp
            appendInstruction (ComparisonInstruction 
                $ CmpInstruction Lt OpInt reg reg reg_1)
            setRegisterCounter reg_1
loadExp reg (Op_less_eq lExp rExp)
    = 
        do
            loadExp reg lExp
            reg_1 <- getRegisterCounter
            loadExp reg_1 rExp
            appendInstruction (ComparisonInstruction 
                $ CmpInstruction Le OpInt reg reg reg_1)
            setRegisterCounter reg_1
loadExp reg (Op_large lExp rExp)
    = 
        do
            loadExp reg lExp
            reg_1 <- getRegisterCounter
            loadExp reg_1 rExp
            appendInstruction (ComparisonInstruction 
                $ CmpInstruction Gt OpInt reg reg reg_1)
            setRegisterCounter reg_1   
loadExp reg (Op_large_eq lExp rExp)
    = 
        do
            loadExp reg lExp
            reg_1 <- getRegisterCounter
            loadExp reg_1 rExp
            appendInstruction (ComparisonInstruction 
                $ CmpInstruction Ge OpInt reg reg reg_1)
            setRegisterCounter reg_1            
loadExp reg (Op_add lExp rExp)
    = 
        do
            loadExp reg lExp
            reg_1 <- getRegisterCounter
            loadExp reg_1 rExp
            appendInstruction (ArithmeticInstruction
                $ Add OpInt reg reg reg_1)
            setRegisterCounter reg_1
loadExp reg (Op_sub lExp rExp)
    = 
        do
            loadExp reg lExp
            reg_1 <- getRegisterCounter
            loadExp reg_1 rExp
            appendInstruction (ArithmeticInstruction
                $ Sub OpInt reg reg reg_1)
            setRegisterCounter reg_1 
loadExp reg (Op_mul lExp rExp)
    = 
        do
            loadExp reg lExp
            reg_1 <- getRegisterCounter
            loadExp reg_1 rExp
            appendInstruction (ArithmeticInstruction
                $ Mul OpInt reg reg reg_1)
            setRegisterCounter reg_1    
loadExp reg (Op_div lExp rExp)
    = 
        do
            loadExp reg lExp
            reg_1 <- getRegisterCounter
            loadExp reg_1 rExp
            appendInstruction (ArithmeticInstruction
                $ Div OpInt reg reg reg_1)
            setRegisterCounter reg_1         
loadExp reg (Op_not exp)
    = 
        do
            loadExp reg exp
            appendInstruction (LogicInstruction
                $ LogicNot reg reg)
loadExp reg (Op_neg exp)
    = 
        do
            loadExp reg exp
            appendInstruction (ArithmeticInstruction
                $ Neg OpInt reg reg)



loadVal :: Int -> LValue -> SymTableState ()
loadVal reg lValue
    = 
        do
            loadVarAddress reg lValue
            appendInstruction (StackInstruction $ LoadIndirect reg reg)

storeVal :: Int -> LValue -> SymTableState ()
-- 目前不管是value还是reference都是按照store_indirect来做（参考Appendix B）
storeVal reg lValue
    = 
        do
            reg_1 <- getRegisterCounter
            loadVarAddress reg_1 lValue
            appendInstruction (StackInstruction $ StoreIndirect reg_1 reg)
            setRegisterCounter reg_1

loadVarAddress :: Int -> LValue -> SymTableState ()
loadVarAddress reg (LId ident)
    =
        do
            (byValue, slotNum, _, _) <- getVariableType ident
            if byValue
            -- load the address of the slot use loadAddress
            then appendInstruction (StackInstruction $ LoadAddress reg slotNum)
            -- load the address directly as the slot store reference(address)
            else appendInstruction (StackInstruction $ Load reg slotNum)

-- TODO: other left values
loadVarAddress reg _ 
    = appendInstruction (Comment "this is an undefined left value")         

-- -- transfer operation from Exp in AST to Oz instruction
-- getOzInstruction :: Exp -> OzInstruction
-- getOzInstruction Op_add = Add
-- getOzInstruction Op_sub = Sub
-- getOzInstruction Op_mul = Mul
-- getOzInstruction Op_div = Div
-- getOzInstruction Op_eq  = Eq
-- getOzInstruction Op_less  = Lt
-- getOzInstruction Op_less_eq  = Le
-- getOzInstruction Op_large  = Gt
-- getOzInstruction Op_large_eq  = Ge
-- getOzInstruction Op_neg  = Neg

-- AssignVar :: Int -> DVar -> Generator ()

getType :: Exp -> SymTableState (Btype)
getType (BoolConst _) = return Bool 
getType (IntConst _) = return Int
getType (StrConst _) = return String
getType (Op_or _ _) = return Bool 
getType (Op_and _ _) = return Bool
getType (Op_eq _ _) = return Bool 
getType (Op_neq _ _) = return Bool
getType (Op_less _ _) = return Bool
getType (Op_less_eq _ _) = return Bool
getType (Op_large _ _) = return Bool
getType (Op_large_eq _ _) = return Bool
getType (Op_add _ _) = return Int
getType (Op_sub _ _) = return Int
getType (Op_mul _ _) = return Int
getType (Op_div _ _) = return Int
getType (Op_not _) = return Bool
getType (Op_neg _) = return Int
-- TODO: need to update later!!!!!!!!!!!!!
getType (Lval (LId ident))
    = 
        do
            (_, _, varType, _) <- getVariableType ident
            let result = case varType of BooleanVar -> Bool
                                         IntegerVar -> Int
            return result

getType (Lval _) = return Int

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0


