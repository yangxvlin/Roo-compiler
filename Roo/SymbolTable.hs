-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yangm, Wenrui Zhang             --
--  Implemented by Team: GNU_project                     --
-----------------------------------------------------------
module SymbolTable where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Text.Parsec.Pos
import RooAST

-- ---------------------------------------------------------------------------
-- Termonology:
-- - global type table: holds information about type aliases and the composite
--   types then name;
--    - att: global array type table
--    - rtt: global record type table
--    - rft: global record field table
-- - global procedure table: holds procedure parameter type, whether by 
--   reference information
--    - pt: global procedure table
--    - pdt: global procedure definition table
-- - local variable table: which provides information about formal parameters 
--   and variables in the procedure that is currently being processed.
--    - lts: stack of local variable tables
-- ---------------------------------------------------------------------------

data CompositeKey = CompositeKey String String
  deriving (Show, Eq, Ord)

-- type SymTableState a = State SymTable a
type SymTableState a = StateT SymTable (Either String) a

data SymTable 
  = SymTable 
    -- global array type table
    { att :: Map String (Int, DataType) 
      -- map of array name with array size and 
      -- data type (bool or integer or record)
    -- global record type table
    , rtt :: Map String (Int) 
      -- map of record name with number of fields
    -- global record field table
    , rft :: Map CompositeKey (BaseType)
      -- map of (record name, field name) with field type
    -- global procedure table
    , pt  :: Map String ([(Bool, DataType)])
    -- global procedure definition table
    , pdt :: Map String (Procedure)
      -- map of procedure name with procedure's definition
    -- , lts :: [LocalVariableTable]
    }

initialSymTable :: SymTable
initialSymTable = SymTable { att = Map.empty
                           , rtt = Map.empty
                           , rft = Map.empty
                           , pt  = Map.empty
                           , pdt = Map.empty
                           }

-- TO DO: PLEASE CHECK FOLLOWING --wenruiz                           
data VarTable = VarTable {lts :: Map String (DataType)}
initialVariableTable :: VarTable {lts = Map.empty}

-- ---------------------------------------------------------------------------
-- TypeTable related helper methods
-- ---------------------------------------------------------------------------

insertArrayType :: Array -> SymTableState ()
insertArrayType (Array arraySize dataType arrayName) 
  = 
    do
      st <- get
      -- duplicate array definition
      if (Map.member arrayName (att st)) then
        liftEither $ throwError ("Duplicated array name: " ++ arrayName)
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
        liftEither $ throwError $ "Duplicated record name: " ++ recordName
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
        liftEither $ throwError $ "Duplicated record field: " ++ 
                                  recordName ++ "." ++ fieldName
      -- insert a (record name, field name) definition
      else
        put $ st { rft = Map.insert ck baseType (rft st) }


-- ---------------------------------------------------------------------------
-- --------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- ProcedureTable related helper methods
-- ---------------------------------------------------------------------------
insertProcedure :: Procedure -> SymTableState ()
insertProcedure (Procedure (ProcedureHeader ident params) (ProcedureBody _ _ ))
  = do
    let formalParams = createformalParams params
    putProcedure ident formalParams

putProcedure :: String -> [(Bool, DataType)] -> SymTableState ()
putProcedure procedureName formalParams
  = do
      st <- get
      -- duplicate record definition
      if (Map.member procedureName (pt st)) then
        liftEither $ throwError $ "Duplicated procedure name: " ++ 
                                  procedureName
      -- insert a record definition
      else
        put $ st { pt =  Map.insert procedureName formalParams (pt st) }

-- | Check if the procedure exists
-- checkProcedure :: String -> SymTableState String
-- checkProcedure procedureName 
--   = do
--       st <- get
--       if (M.member procedureName (pt st)) then 
--         return $ (pt st) M.! procedureName
--       else 
--         liftEither $ throwError $ "Procedure named " ++ procedureName ++ 
--                                   " does not exist"

createformalParams :: [Parameter] -> [(Bool, DataType)]
createformalParams [] = []
createformalParams (r:rs) 
  = case r of
      BooleanVal _ -> [(True, BasyDataType BooleanType)] ++ (createformalParams rs)
      IntegerVal _ -> [(True, BasyDataType IntegerType)] ++ (createformalParams rs)
      DataParameter dataType _ -> [(False, dataType)] ++ (createformalParams rs)

insertProcedureDefinition :: Procedure -> SymTableState ()
insertProcedureDefinition p@(Procedure 
                              (ProcedureHeader procedureName params) 
                              (ProcedureBody _ _ )
                            )
  = 
    do
      st <- get
      -- duplicate procedure definition
      if (Map.member procedureName (pdt st)) then 
        liftEither $ throwError $ "Duplicated procedure name: " ++ 
                                  procedureName
      -- insert a procedure definition
      else 
        put $ st { pdt = Map.insert procedureName p (pdt st) }

getProcedureDefinition :: String -> SymTableState Procedure
getProcedureDefinition procedureName
  =
    do
      st <- get
      if (Map.member procedureName (pdt st)) then 
        return $ (pdt st) Map.! procedureName
      else 
        liftEither $ throwError $ "Procedure named " ++ procedureName ++ 
                                  " does not exist"


-- ---------------------------------------------------------------------------
-- VariableTable related helper methods
-- --------------------------------------------------------------------------- 

-- TO DO: PLEASE CHECK FOLLOWING --wenruiz
-- insert one variable to the given procedure
insertVariable :: Procedure -> VariableDecl -> SymTableState ()
insertVariable (Procedure (ProcedureHeader ident params) (ProcedureBody _ _ )) (VariableDecl DataType Ident)
  = do
      -- extract the variable map from the procedure
      
      -- duplicate variable definition
      if (Map.member arrayName (att st)) then
        error $ "Duplicated variable name: " ++ arrayName
      -- insert an variable definition
      else
        put $ st { att =  Map.insert arrayName (arraySize, dataType) (att st) }