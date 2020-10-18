-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yang, Wenrui Zhang             --
--  Implemented by Team: GNU_project                     --
-----------------------------------------------------------
module RooAnalyser(analyse, Result(..)) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import RooAST
import SymbolTable
import qualified Data.Map as Map
import Data.Either

type Result = Either String SymTable

analyse :: Program -> Result 
analyse prog
  = evalStateT (semanticCheckRooProgram prog) initialSymTable

semanticCheckRooProgram :: Program -> SymTableState SymTable
semanticCheckRooProgram prog
  =
    do
      constructSymbolTable prog
      checkDistinctTypeAlias
      mainProcedure <- getProcedureDefinition "main"
      st <- get
      return st


constructSymbolTable :: Program -> SymTableState ()
constructSymbolTable prog@(Program records arraies procedures)
  = 
    do
      st <- get
      mapM_ insertRecordType records
      mapM_ insertArrayType arraies
      mapM_ insertProcedureDefinition procedures

-- all type aliases must be distinct, record and array has no overlapping name
checkDistinctTypeAlias :: SymTableState ()
checkDistinctTypeAlias
  =
    do
      st <- get
      -- has overlapping name in type alias
      if Map.size (Map.intersection (att st) (rtt st)) > 0 then
        liftEither $ throwError ("Duplicated type alias name")
      else
        return ()

-- Within a given procedure, variable names, including formal parameter names, 
-- must be distinct.

-- procedure's parameter and local variable should have different name
