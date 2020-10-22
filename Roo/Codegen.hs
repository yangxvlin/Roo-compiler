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
generateProcedure p@(Procedure (ProcedureHeader procID _) (ProcedureBody _ stmts))
    =
        do
            appendInstruction $ Label procID 

            pushLocalVariableTable
            -- insert procedure's variable info to local variable table
            insertProcedureVariable p
            -- generate code of procedure's statements
            slotNum <- getSlotCounter
            appendInstruction (StackInstruction $ PushStackFrame slotNum)

            -- appendInstruction $ StackInstruction
            mapM_ generateStatement stmts

            -- end of the procedure
            appendInstruction (StackInstruction $ PopStackFrame slotNum)
            appendInstruction (ProcedureInstruction IReturn)

            popLocalVariableTable

           

generateStatement :: Stmt -> SymTableState ()
generateStatement (Write exp)
    = 
        do
            appendInstruction $ Comment $ "Write " ++ show exp
            let bType = getType exp
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
            appendInstruction $ Comment $ "Writeln " ++ show exp
            let bType = getType exp
            reg <- getRegisterCounter
            loadExp reg exp
            let cmd = case bType of Int -> "print_int"
                                    Bool -> "print_bool"
                                    String -> "print_string"
            appendInstruction (ProcedureInstruction $ ICallBuiltIn cmd)
            appendInstruction (ProcedureInstruction $ ICallBuiltIn "print_newline")
            setRegisterCounter reg
-- need to update
generateStatement _ 
    = appendInstruction (Comment "this is an undefined statment")

-- load an expression to the given register
loadExp :: Int -> Exp -> SymTableState ()
loadExp reg (BoolConst vl) 
    = appendInstruction (ConstantInstruction $ OzIntConst reg $ boolToInt vl)
loadExp reg (IntConst vl)
    = appendInstruction (ConstantInstruction $ OzIntConst reg vl)
loadExp reg (StrConst vl)
    = appendInstruction (ConstantInstruction $ OzStringConst reg vl)
loadExp reg _ 
    = appendInstruction (Comment "this is an undefined expression")


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


getType :: Exp -> Btype
getType (BoolConst _) = Bool 
getType (IntConst _) = Int
getType (StrConst _) = String
getType (Op_or _ _) = Bool 
getType (Op_and _ _) = Bool
getType (Op_eq _ _) = Bool 
getType (Op_neq _ _) = Bool
getType (Op_less _ _) = Bool
getType (Op_less_eq _ _) = Bool
getType (Op_large _ _) = Bool
getType (Op_large_eq _ _) = Bool
getType (Op_add _ _) = Int
getType (Op_sub _ _) = Int
getType (Op_mul _ _) = Int
getType (Op_div _ _) = Int
getType (Op_not _) = Bool
getType (Op_neg _) = Int
-- need to update later!!!!!!!!!!!!!
getType (Lval _) = Int

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0
