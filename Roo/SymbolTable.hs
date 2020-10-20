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
-- 0. st: symbol table
-- 1. global type table: holds information about type aliases and the composite
--    types then name;
--     a) att: global alias type table
--     b) rft: global record field table
-- 2. global procedure table: holds procedure parameter type, whether by 
--    reference information
--    a) pt: global procedure table
-- 3. local variable table: which provides information about formal parameters 
--    and variables in the procedure that is currently being processed.
--    a) lts: stack of local variable tables
--    b) vtt: variable type table
--    c) cvt: current procedure's variable table
-- ---------------------------------------------------------------------------

data CompositeKey = CompositeKey String String
  deriving (Show, Eq, Ord)

type SymTableState a = StateT SymTable (Either String) a

-- A short hand form for variable type
data VariableType = BooleanVar 
                  | IntegerVar 
                  | RecordVar String 
                  | ArrayVar String

-- 1. available slot number
-- 2. available register number
-- 3. mapping of variable name with 
--    a) true if it is pass by value and
--    b) allocated slot number and
--    c) its type and
--    d) #elements for array, #fields for record, 1 for boolean/integer
data LocalVariableTable
  = LocalVariableTable
    { slotCounter :: Int
    , registerCounter :: Int
    , vtt :: Map String (Bool, Int, VariableType, Int)
    }

-- Array:  array size, type
-- Record: #fields,    [field's definition]
data AliasTypeInfo 
  = ArrayInfo  (Int, DataType)
  | RecordInfo (Int, [FieldDecl])

-- 1. att  :: global alias type table
-- 2. rft  :: global record field table
--            = map of (record name, field name) with 
--                a) field's type and
--                b) index of the field in record
-- 3. pt   :: global procedure table
--            = map of procedure name with
--                a) [true if pass by value, parameter's type]
--                b) procedure's deifnition
-- 4. lvts :: stack of local variable table  
data SymTable 
  = SymTable 
    { att :: Map String AliasTypeInfo
    , rft :: Map CompositeKey (BaseType, Int)
    , pt  :: Map String ([(Bool, DataType)], Procedure) 
    , lvts :: [LocalVariableTable]
    , labelCounter :: Int
    }

initialSymTable :: SymTable
initialSymTable = SymTable { att = Map.empty
                           , rft = Map.empty
                           , pt  = Map.empty
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
        liftEither $ throwError ("Duplicated alias type: " ++ arrayName)
      -- insert an array definition
      else
        put $ st { att =  Map.insert arrayName 
                                     (ArrayInfo (arraySize, dataType)) 
                                     (att st) 
                 }

getArrayType :: String -> SymTableState (Int, DataType)
getArrayType arrayName
  = 
    do
      st <- get
      -- get an array definition
      if (Map.member arrayName (att st)) then
        let (ArrayInfo info) = ((att st) Map.! arrayName) in return info
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
      if (Map.member recordName (att st)) then
        liftEither $ throwError $ "Duplicated alias type: " ++ recordName
      -- insert a record definition
      else
        do
          insertRecordFields recordName fieldDecls 0
          put $ st { att = Map.insert recordName 
                                      (RecordInfo (recordSize, fieldDecls))
                                      (att st) 
                   }

getRecordType :: String -> SymTableState (Int, [FieldDecl])
getRecordType recordName
  = 
    do
      st <- get
      -- get an record definition
      if (Map.member recordName (att st)) then
        let (RecordInfo info) = (att st) Map.! recordName in return info
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
          case (att st) Map.! typeName of 
            (RecordInfo _) -> return (RecordVar typeName)
            (ArrayInfo  _) -> return (ArrayVar  typeName)
      else 
        liftEither $ throwError $ "Undefiend alias type: " ++ typeName

-- ---------------------------------------------------------------------------
-- ProcedureTable related helper methods
-- ---------------------------------------------------------------------------
insertProcedure :: Procedure -> SymTableState ()
insertProcedure p@(Procedure (ProcedureHeader ident params) _)
  = do
    let formalParams = map createformalParam params
    putProcedure ident formalParams p

putProcedure :: String -> [(Bool, DataType)] -> Procedure -> SymTableState ()
putProcedure procedureName formalParams p
  = do
      st <- get
      -- duplicate procedure definition
      if (Map.member procedureName (pt st)) then
        liftEither $ throwError $ "Duplicated procedure name: " ++ 
                                  procedureName
      -- insert a procedure definition
      else
        put $ st {pt = Map.insert procedureName (formalParams, p) (pt st)}

-- get procedure's type info
getProcedure :: String -> SymTableState ([(Bool, DataType)], Procedure)
getProcedure procedureName 
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

updateCurVariableTable :: LocalVariableTable -> SymTableState ()
updateCurVariableTable newLVT
  =
    do
      popLocalVariableTable
      st <- get
      let newLvts = (lvts st) ++ [newLVT]
      put $ st { lvts = newLvts }

-- check variable name not exist in the local variable table as key
checkVariableNotDefined :: String -> SymTableState ()
checkVariableNotDefined varName
  =
    do
      cvt <- getCurVariableTable
      if (Map.member varName (vtt cvt)) then 
        liftEither $ throwError $ "Duplicated variable name: " ++ varName
      else 
        return ()

getVariableType :: String -> SymTableState (Bool, Int, VariableType, Int)
getVariableType varName
  =
    do
      cvt <- getCurVariableTable
      if (Map.member varName (vtt cvt)) then
         return $ (vtt cvt) Map.! varName
      else 
        liftEither $ throwError $ "Unknown variable name: " ++ varName

getVarRecordField :: String -> String -> SymTableState (BaseType, Int)
getVarRecordField varName fieldName
  =
    do
      st <- get
      cvt <- getCurVariableTable
      (_, _, varType, _) <- getVariableType varName
      case varType of
        (RecordVar recordName) -> getRecordField recordName varName
        _ -> liftEither $ throwError $ "Variable name: " ++ varName ++ 
                                       " is not field type"

-- ---------------------------------------------------------------------------
-- VariableTable construction methods
-- --------------------------------------------------------------------------- 

insertProcedureVariable :: Procedure -> SymTableState ()
insertProcedureVariable (Procedure (ProcedureHeader ident params) 
                                   (ProcedureBody variableDecls _ ))
  =
    do
      mapM_ insertProcedureParameter params
      mapM_ insertProcedureVariableDecl variableDecls

insertProcedureParameter :: Parameter -> SymTableState ()
insertProcedureParameter (BooleanVal varName) 
  = insertVariable BooleanVar True varName
insertProcedureParameter (IntegerVal varName) 
  = insertVariable IntegerVar True varName
insertProcedureParameter (DataParameter (BaseDataType BooleanType) 
                                        varName) 
  = insertVariable BooleanVar False varName
insertProcedureParameter (DataParameter (BaseDataType IntegerType) 
                                        varName) 
  = insertVariable IntegerVar False varName
insertProcedureParameter (DataParameter (AliasDataType typeName) 
                                        varName)
  =
    do
      aliasType <- getTypeAlias typeName
      insertVariable aliasType False varName

insertProcedureVariableDecl :: VariableDecl -> SymTableState ()
insertProcedureVariableDecl (VariableDecl (BaseDataType BooleanType) 
                                          variableNames)
  =
    do
      mapM_ (insertVariable BooleanVar False) variableNames
insertProcedureVariableDecl (VariableDecl (BaseDataType IntegerType) 
                                          variableNames)
  =
    do
      mapM_ (insertVariable IntegerVar False) variableNames
insertProcedureVariableDecl (VariableDecl (AliasDataType typeName) 
                                          variableNames)
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
insertVariable recVar@(RecordVar recordName) byValue varName
  =
    do
      checkVariableNotDefined varName
      cvt <- getCurVariableTable
      let availableSlot = slotCounter cvt
      (recordSize, _) <- getRecordType recordName
      let newSlotCounter = availableSlot + recordSize
      updateNewVariableToLVT newSlotCounter 
                             varName 
                             (byValue, availableSlot, recVar, recordSize)
insertVariable arr@(ArrayVar arrayName) byValue varName
  =
    do
      checkVariableNotDefined varName
      cvt <- getCurVariableTable
      let availableSlot = slotCounter cvt
      (arraySize, arrayType) <- getArrayType arrayName
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
