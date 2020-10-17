-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yang                            --
--  Implemented by Team: GNU_project                     --
-----------------------------------------------------------
module RooAnalyser(analyse, Result(..)) where

import RooAST
import SymbolTable

data Result = Okay SymTable
            | Err String

analyse :: Program -> Result
analyse (Program records arraies procedures)
  = 
    -- do
      -- runState initialSymTable
      Okay (SymTable {  } )
