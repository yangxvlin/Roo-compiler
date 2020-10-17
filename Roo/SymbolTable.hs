-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yangm, Wenrui Zhang             --
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
--  att: global array type table
--  rtt: global record type table
--  rft: global record field table
-- ---------------------------------------------------------------------------

data CompositeKey = CompositeKey String String
  deriving (Show, Eq, Ord)

type SymTableState a = State SymTable a

data SymTable 
  = SymTable 
    -- global array type table
    { att :: Map String (Int, DataType) 
      -- map of array name with array size and 
      --                        data type (bool or integer or record)
    -- global record type table
    , rtt :: Map String (Int) 
      -- map of record name with number of fields
    -- global record field table
    , rft :: Map CompositeKey (BaseType)
      -- map of (record name, field name) with field type
    -- , pt :: ProcedureTable
    -- , lts :: [LocalVariableTable]
    }

initialSymTable :: SymTable
initialSymTable = SymTable { att = Map.empty
                           , rtt = Map.empty
                           , rft = Map.empty
                           }

-- ---------------------------------------------------------------------------
-- TypeTable related data structure and helper methods
-- ---------------------------------------------------------------------------

insertArrayType :: Array -> SymTableState ()
insertArrayType (Array arraySize dataType arrayName) 
  = 
    do
      st <- get
      -- duplicate array definition
      if (Map.member arrayName (att st)) then
        error $ "Duplicated array name: " ++ arrayName
      -- insert an array definition
      else
        put $ st { att =  Map.insert arrayName (arraySize, dataType) (att st) }

-- assume fieldDecls are not duplicate
insertRecordType :: Record -> SymTableState ()
insertRecordType (Record fieldDecls recordName)
  = 
    do
      st <- get
      let recordSize = length fieldDecls
      -- duplicate record definition
      if (Map.member recordName (rtt st)) then
        error $ "Duplicated record name: " ++ recordName
      -- insert a record definition
      else
        do
          mapM_ (insertRecordFields recordName) fieldDecls 
          put $ st { rtt = Map.insert recordName recordSize (rtt st) }

insertRecordFields :: String -> FieldDecl -> SymTableState ()
insertRecordFields recordName (FieldDecl baseType fieldName) 
  = 
    do
      st <- get
      let ck = CompositeKey recordName fieldName
      -- duplicate (record name, field name) definition
      if (Map.member ck (rft st)) then
        error $ "Duplicated record field: " ++ recordName ++ "." ++ fieldName
      -- insert a (record name, field name) definition
      else
        put $ st { rft = Map.insert ck baseType (rft st) }


-- ---------------------------------------------------------------------------
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- procedure table related data structure and helper methods
-- ---------------------------------------------------------------------------

