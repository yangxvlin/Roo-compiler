-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yang                            --
--  Implemented by Team: GNU_project                     --
-----------------------------------------------------------
module SymbolTable where

import Control.Monad.State
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Text.Parsec.Pos
import RooAST

-- a (global) "type table" which holds information about type aliases and the 
-- composite types then name;
data TypeTable 
  = TypeTable
    -- map of array name with array size and 
    --                        data type (bool or integer or record)
    { arrayType :: Map String (Int, DataType) 
    -- map of record name with field declaration 
    --                             (map of field's name with bool or integer) 
    --                         and number of fields
    , recordType :: Map String ((Map String BaseType), Int)
    }
