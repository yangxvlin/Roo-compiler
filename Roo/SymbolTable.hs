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
--    - att: global alias type table
--    - rtt: global record type table
--    - rft: global record field table
-- - global procedure table: holds procedure parameter type, whether by 
--   reference information
--    - pt: global procedure table
--    - pdt: global procedure definition table, holds procedure's defintion
-- - local variable table: which provides information about formal parameters 
--   and variables in the procedure that is currently being processed.
--    - lts: stack of local variable tables
--    - vtt: variable type table
--    - cvt: current procedure's variable table
-- ---------------------------------------------------------------------------

data CompositeKey = CompositeKey String String
  deriving (Show, Eq, Ord)

type SymTableState a = StateT SymTable (Either String) a

data VariableType = BooleanVar | IntegerVar | RecordVar String | ArrayVar String

data LocalVariableTable
  = LocalVariableTable
    -- available slot number
    { slotCounter :: Int
    -- available register number
    , registerCounter :: Int
    -- mapping of variable name with whether it is pass by value and
    --                               allocated slot number and
    --                               its type and
    --                               #elements for array, 
    --                                1 for bool/int/record.field
    , vtt :: Map String (Bool, Int, VariableType, Int)
      -- only true if procedure's parameter is pass by value
    }

-- Array:  array size, type
-- Record: #fields,    [field's definition]
data AliasTypeInfo 
  = Array  (Int, DataType)
  | Record (Int, [FieldDecl])

data SymTable 
  = SymTable 
    -- global alias type table
    { att :: Map String AliasTypeInfo
    -- global record field table
    , rft :: Map CompositeKey (BaseType, Int)
      -- map of (record name, field name) with field type and
      --                                       its index in record
    -- global procedure table
    , pt  :: Map String ([(Bool, DataType)]) 
      -- only true if procedure's parameter is pass by value
    -- global procedure definition table
    , pdt :: Map String (Procedure)
    -- stack of local variable table  
    , lvts :: [LocalVariableTable]
    -- available label number
    , labelCounter :: Int
    }

initialSymTable :: SymTable
initialSymTable = SymTable { att = Map.empty
                           , rft = Map.empty
                           , pt  = Map.empty
                           , pdt = Map.empty
                           , lvts = []
                           , labelCounter = 0
                           }

initialLocalVariableTable :: LocalVariableTable
initialLocalVariableTable = LocalVariableTable
  { slotCounter     = 0
  , registerCounter = 0
  , vtt             = Map.empty
  }

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
        put $ st { att =  Map.insert arrayName 
                                     (Array (arraySize, dataType)) 
                                     (att st) 
                 }

getArrayType :: String -> SymTableState (Int, DataType)
getArrayType arrayName
  = 
    do
      st <- get
      -- get an array definition
      if (Map.member arrayName (att st)) then
        let (Array info) = ((att st) Map.! arrayName) in return info
      -- no array definition
      else
        liftEither $ throwError $ "Array named " ++ arrayName ++ 
                                  " does not exist"

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
          insertRecordFields recordName fieldDecls 0
          put $ st { rtt = Map.insert recordName 
                                      (Record (recordSize, fieldDecls))
                                      (rtt st) 
                   }

getRecordType :: String -> SymTableState (Int, [FieldDecl])
getRecordType recordName
  = 
    do
      st <- get
      -- get an record definition
      if (Map.member recordName (rtt st)) then
        let (Record info) = (rtt st) Map.! recordName in return info
      -- no record definition
      else
        liftEither $ throwError $ "Record named " ++ recordName ++ 
                                  " does not exist"

insertRecordFields :: String -> [FieldDecl] -> Int -> SymTableState ()
insertRecordFields _ [] _ = return ()
insertRecordFields recordName (x:xs) index
  =
    do
      insertRecordField recordName x index
      insertRecordFields recordName xs (index+1)

insertRecordField :: String -> FieldDecl -> Int -> SymTableState ()
insertRecordField recordName (FieldDecl baseType fieldName) index
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
        put $ st { rft = Map.insert ck (baseType, index) (rft st) }

getRecordField :: String -> String -> SymTableState (BaseType, Int)
getRecordField recordName fieldName
  = 
    do
      st <- get
      let ck = CompositeKey recordName fieldName
      -- get a (record name, field name) definition
      if (Map.member ck (rft st)) then
        return $ (rft st) Map.! ck
      -- no (record name, field name) definition
      else
        liftEither $ throwError $ "Record.field: " ++ 
                                  recordName ++ "." ++ fieldName ++ 
                                  " does not exist"

getTypeAlias :: String -> SymTableState VariableType
getTypeAlias typeName
  =
    do
      st <- get
      if (Map.member typeName (att st)) then 
        do
          case (rtt st) Map.! typeName of 
            (Record _) -> return (RecordVar typeName)
            (Array  _) -> return (ArrayVar  typeName)
      else 
        liftEither $ throwError $ "Undefiend alias type: " ++ typeName

-- ---------------------------------------------------------------------------
-- ProcedureTable related helper methods
-- ---------------------------------------------------------------------------
insertProcedure :: Procedure -> SymTableState ()
insertProcedure (Procedure (ProcedureHeader ident params) (ProcedureBody _ _ ))
  = do
    let formalParams = map createformalParam params
    putProcedure ident formalParams

putProcedure :: String -> [(Bool, DataType)] -> SymTableState ()
putProcedure procedureName formalParams
  = do
      st <- get
      -- duplicate procedure definition
      if (Map.member procedureName (pt st)) then
        liftEither $ throwError $ "Duplicated procedure name: " ++ 
                                  procedureName
      -- insert a procedure definition
      else
        put $ st { pt =  Map.insert procedureName formalParams (pt st) }

-- get procedure's parameter by name
getProcedureParams :: String -> SymTableState [(Bool, DataType)]
getProcedureParams procedureName 
  = do
      st <- get
      if (Map.member procedureName (pt st)) then 
        return $ (pt st) Map.! procedureName
      else 
        liftEither $ throwError $ "Procedure named " ++ procedureName ++ 
                                  " does not exist"

-- convert Parameter definted in AST to a tuple (is passed by value, type)
createformalParam :: Parameter -> (Bool, DataType)
createformalParam (BooleanVal _) = (True, BaseDataType BooleanType)
createformalParam (IntegerVal _) = (True, BaseDataType IntegerType)
createformalParam (DataParameter dataType _) = (False, dataType)


insertProcedureDefinition :: Procedure -> SymTableState ()
insertProcedureDefinition p@(Procedure (ProcedureHeader procedureName _)  _)
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
pushLocalVariableTable :: SymTableState ()
pushLocalVariableTable
  =
    do
      st <- get
      let newLvts = (lvts st) ++ [initialLocalVariableTable]
      put $ st { lvts = newLvts }

popLocalVariableTable :: SymTableState ()
popLocalVariableTable
  =
    do
      st <- get
      let newLvts = init (lvts st)
      put $ st { lvts = newLvts }

getCurVariableTable :: SymTableState LocalVariableTable
getCurVariableTable
  =
    do
      st <- get
      return $ last $ lvts st

putProcedureVar :: [Parameter] -> SymTableState ()
putProcedureVar [] = []
putProcedureVar formalParams
  = do
      st <- get
      lastVarTable <- last (lvts st)
      -- duplicate record definition
      if (Map.member procedureName lastVarTable) then
        liftEither $ throwError $ "Duplicated procedure name: " ++ procedureName
      -- insert a record definition
      else
        put $ st { pt =  Map.insert procedureName formalParams (pt st) }

-- ---------------------------------------------------------------------------
-- VariableTable construction methods
-- --------------------------------------------------------------------------- 

insertProcedureVariable :: Procedure -> SymTableState ()
insertProcedureVariable (Procedure (ProcedureHeader ident params) 
                                   (ProcedureBody variableDecls _ )
                        )
  =
    do
      pushLocalVariableTable
      -- current local variable table
      let cvt = getCurVariableTable
      mapM_ insertProcedureParameter params
      mapM_ insertProcedureVariableDecl variableDecls

insertProcedureParameter :: Parameter -> SymTableState ()
insertProcedureParameter (BooleanVal varName) 
  = insertVariable BooleanVar True varName
insertProcedureParameter (IntegerVal varName) 
  = insertVariable IntegerVar True varName
insertProcedureParameter (DataParameter (BaseDataType BooleanType) 
                                         varName
                         ) 
  = insertVariable BooleanVar False varName
insertProcedureParameter (DataParameter (BaseDataType IntegerType) 
                                         varName
                         ) 
  = insertVariable IntegerVar False varName
insertProcedureParameter (DataParameter (AliasDataType typeName) 
                                         varName
                         )
  =
    do
      aliasType <- getTypeAlias typeName
      insertVariable aliasType False varName

insertProcedureVariableDecl :: VariableDecl -> SymTableState ()
insertProcedureVariableDecl (VariableDecl (BaseDataType BooleanType) 
                                          variableNames
                            )
  =
    do
      mapM_ (insertVariable BooleanVar False) variableNames
insertProcedureVariableDecl (VariableDecl (BaseDataType IntegerType) 
                                          variableNames
                            )
  =
    do
      mapM_ (insertVariable IntegerVar False) variableNames
insertProcedureVariableDecl (VariableDecl (AliasDataType typeName) 
                                          variableNames
                            )
  =
    do
      aliasType <- getTypeAlias typeName
      mapM_ (insertVariable aliasType False) variableNames

insertVariable ::  VariableType -> Bool -> String -> SymTableState ()
insertVariable BooleanVar byValue varName
  =
    do
      checkVariableNotDefined varName
      cvt <- getCurVariableTable
      let availableSlot = slotCounter cvt
      let newSlotCounter = availableSlot + 1
      updateNewVariableToLVT newSlotCounter 
                             varName 
                             (byValue, availableSlot, BooleanVar, 1)
insertVariable IntegerVar byValue varName
  =
    do
      checkVariableNotDefined varName
      cvt <- getCurVariableTable
      let availableSlot = slotCounter cvt
      let newSlotCounter = availableSlot + 1
      updateNewVariableToLVT newSlotCounter 
                             varName 
                             (byValue, availableSlot, IntegerVar, 1)
insertVariable recVar@(RecordVar typeName) byValue varName
  =
    do
      checkVariableNotDefined varName
      cvt <- getCurVariableTable
      let availableSlot = slotCounter cvt
      (recordSize, _) <- getRecordType typeName
      let newSlotCounter = availableSlot + recordSize
      updateNewVariableToLVT newSlotCounter 
                             varName 
                             (byValue, availableSlot, recVar, recordSize)
insertVariable arr@(ArrayVar typeName) byValue varName
  =
    do
      checkVariableNotDefined varName
      cvt <- getCurVariableTable
      let availableSlot = slotCounter cvt
      (arraySize, arrayType) <- getArrayType typeName
      case arrayType of
        BaseDataType _ ->
          do
            let newSlotCounter = availableSlot + arraySize
            updateNewVariableToLVT newSlotCounter 
                                   varName 
                                   (byValue, availableSlot, arr, arraySize)
        -- as an array cannot has alias type: array 
        AliasDataType recordName ->
          do
            (recordSize, _) <- getRecordType recordName
            -- array size * #record's fields
            let nSlotsRequired = recordSize * arraySize
            let newSlotCounter = availableSlot + nSlotsRequired
            updateNewVariableToLVT newSlotCounter 
                                   varName 
                                   (byValue, availableSlot, arr, nSlotsRequired
                                   )

updateNewVariableToLVT :: Int -> String -> (Bool, Int, VariableType, Int) 
                              -> SymTableState ()
updateNewVariableToLVT newSlotCounter 
                       varName 
                       (byValue, availableSlot, varType, slotRequired)
  =
    do
      cvt <- getCurVariableTable
      updateCurVariableTable cvt 
        { slotCounter = newSlotCounter
        , vtt = Map.insert varName 
                           (byValue, availableSlot, varType, slotRequired)
                           (vtt cvt)
        }
