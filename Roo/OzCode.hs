-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yangm, Wenrui Zhang             --
--  Implemented by Team: GNU_project                     --
-----------------------------------------------------------

module OzCode(writeCode) where

-- writeCode :: () -> String
-- writeCode _ = "writeCode"

-- DOUBLE CHECK PLZ --wenruiz
type Register = Int

data stackInstruction
    = pushStackFrame Int
    | popStachFrame Int
    | store Int Register
    | 