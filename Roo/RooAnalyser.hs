-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yangm, Wenrui Zhang             --
--  Implemented by Team: GNU_project                     --
-----------------------------------------------------------
module RooAnalyser(analyse, Result(..)) where

import Control.Monad.Except
import Control.Monad.State
import RooAST
import SymbolTable
import Data.Map

data Result = Okay SymTable
            | Err String

analyse :: Program -> Result 
analyse prog
  = let st = execState (construct prog) initialSymTable in 
      (Err $ show $ size $ att st)
    -- do
    --   let res = execState (construct prog) initialSymTable
    --   case res of
    --     SymTable st
    --       -> return (Okay st)

construct :: Program -> SymTableState ()
construct prog@(Program records arraies procedures)
  = 
    do
      st <- get
      mapM_ insertRecordType records
      mapM_ insertArrayType arraies
      -- return (Okay st)
