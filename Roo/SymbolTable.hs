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

-- ---------------------------------------------------------------------------
-- Termonology:
--  att: array type table
-- ---------------------------------------------------------------------------

data SymTable 
  = SymTable 
    -- global array type table
    { att :: Map String (Int, DataType) 
      -- map of array name with array size and 
      --                        data type (bool or integer or record)
    -- global record type table
    , rtt :: Map String ((Map String BaseType), Int) 
      -- map of record name with field declaration 
      --                             (map of field's name with bool or integer)
      --                         and number of fields
    , pt :: ProcedureTable
    , lts :: [LocalVariableTable]
    }

-- ---------------------------------------------------------------------------
-- TypeTable related data structure and helper methods
-- ---------------------------------------------------------------------------

insertArrayType :: Array -> State SymTable ()
insertArrayType (Array arraySize dataType arrayName) 
  = 
    do
      st <- get
      -- duplicate array definition
      if Map.member arrayName (att st)) then
        error $ "Duplicated array name: " ++ arrayName
      -- insert an array definiotion to att
      else
        put $ st { att =  Map.insert arrayName (arraySize, dataType) (att st) }

insertRecordType :: Record -> State SymTable ()
insertRecordType ()
  =
    do
      st <- get
      -- duplicate array definition
      if Map.member arrayName (att st)) then
        error $ "Duplicated array name: " ++ arrayName
      -- insert an array definiotion to att
      else
        put $ st { att =  Map.insert arrayName (arraySize, dataType) (att st) }

-- ---------------------------------------------------------------------------
-- ---------------------------------------------------------------------------

