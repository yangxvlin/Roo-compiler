-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yang                            --
--  Implemented by Team: GNU_project                     --
-----------------------------------------------------------
module RooAnalyser(analyse, Result(..)) where

import Control.Monad
import Control.Monad.State
import RooAST
import SymbolTable
import qualified Data.Map as Map
import Data.Either

data Result = Okay SymTable
            | Err String


analyse :: Program -> Result 
analyse prog
  = let st = execState (constructSymbolTable prog) initialSymTable in 
      semanticCheck prog (Okay st)

constructSymbolTable :: Program -> SymTableState ()
constructSymbolTable prog@(Program records arraies procedures)
  = 
    do
      st <- get
      mapM_ insertRecordType records
      mapM_ insertArrayType arraies

semanticCheck :: Program -> Result -> Result
semanticCheck prog st 
  = 
    do
      checkHasMainProcedure st

checkHasMainProcedure :: Result -> Result
checkHasMainProcedure (Okay st) = 
  if (Map.member "main" procedureTable) then
    (Okay st)
  else
    (Err "The program should has at least one main procedure!")
  where
    procedureTable = pt st
checkHasMainProcedure e@(Err err) = e