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

ozCode :: SymTable -> Program -> Consequence
ozCode st prog = evalStateT (codeGeneration prog) st

codeGeneration :: Program -> SymTableState [OzInstruction]
codeGeneration _
    = 
        do
            let generatedCode = [ProcedureInstruction $ ICall "proc_main", 
                                ProcedureInstruction $ IHalt]

            mapM_ appendInstruction generatedCode
            -- starts with main procedure
            (_, mainProc) <- getProcedure "main"
            -- codes <- generateProcedure mainProc

            st <- get
            return $ instructions st

-- generateProcedure :: Procedure -> SymTableState [OzInstruction]
-- generateProcedure p@(Procedure (ProcedureHeader procID _) (ProcedureBody _ stmts))
--     =
--         do
--             let generatedCode = [Label procID] 

--             -- 
--             pushLocalVariableTable
--             -- insert procedure's variable info to local variable table
--             insertProcedureVariable p
--             -- generate code of procedure's statements
--             -- reg <- getRegisterCounter
--             -- generatedCode <- generatedCode ++ [StackInstruction $ Load $ reg 10] 
--             let code = concat (mapM generateStatement stmts)

--             popLocalVariableTable

--             return $ generatedCode 

-- generateStatement :: Stmt -> SymTableState [OzInstruction]
-- generateStatement _ 
--     = 
--         do
--             return [Comment "this is a statment"]

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