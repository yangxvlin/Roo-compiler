-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yangm, Wenrui Zhang             --
--  Implemented by Team: GNU_project                     --
-----------------------------------------------------------
module Codegen(ozCode) where

import SymbolTable
import RooAST

ozCode :: SymTable -> Program -> ()
ozCode _ _ = ()

-- assume fieldDecls are not duplicate