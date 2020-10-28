-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yang, Wenrui Zhang, Xu Shi      --
--  Implemented by Team: GNU_project                     --
-----------------------------------------------------------
module RooAnalyser(analyse, Result(..)) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import RooAST
import SymbolTable
import Data.Map (Map, (!),delete)
import qualified Data.Map as Map
import Data.Either

type Result = Either String SymTable

--------------------main function of semantic analysis------------------------

analyse :: Program -> Result
analyse prog
  = evalStateT (semanticCheckRooProgram prog) initialSymTable

------------------- semantic analysis on a roo program------------------------
-- semanticCheckRooProgram
-- 1.insert records arraies and procedures into symbol table
-- 2.Check that there is one and only one main procedure, and its arity is 0
-- 3.Check all procedures

semanticCheckRooProgram :: Program -> SymTableState SymTable
semanticCheckRooProgram prog
  =
    do
      constructSymbolTable prog
      checkArityProcedure "main" 0

      st <- get
      let procedures = pt st
      mapM_ checkOneProcedures procedures
      st2 <- get
      return st2

------------------------------------------------------------------------------
---------------------Semantic checking on a procedures------------------------
-- checkOneProcedures
-- 1.initialize local variable table
-- 2.insert procedure's parameter and variable into lvt
-- 3.check all statements
-- 4.Empty lvt

checkOneProcedures :: ([(Bool, DataType)], Procedure) -> SymTableState ()
checkOneProcedures  (_, procCalled@(Procedure _ (ProcedureBody _ procStmts)))
  =
    do
        pushLocalVariableTable
        insertProcedureVariable procCalled
        checkStmts procStmts
        popLocalVariableTable

------------------------------------------------------------------------------
------------Semantic checking on all statement of a procedure-----------------
checkStmts :: [Stmt] -> SymTableState ()
checkStmts = mapM_ checkStmt


--check one procedure:
checkStmt :: Stmt -> SymTableState ()
-- check assign:
-- 1.Check if lvalue and exp use the correct format
-- 2.check if lvalue and exp have same data type
-- 3. if exp are record or array type, both lvalue and exp
-- pass by reference
checkStmt (Assign lvalue exp)
  =
    do
      checkLValue lvalue
      checkExp exp
      iInfo <- getDataTypeOfLValue lvalue
      let (byV, identi) = iInfo
      expType <- getExpType exp
      let showLValueName = getLValueName lvalue
      let showTypeName = getDataT expType

      if  not (identi == expType) then
        liftEither $ throwError $ "assign a wrong type "
        ++ showTypeName ++ " to " ++ showLValueName
      else
        if expIsLvalue exp then
          do

            let (Lval expLvalue) = exp
            iRA <- lvalueIsRerAry expLvalue
            if iRA then
              do
                iInfo2 <- getDataTypeOfLValue expLvalue--jiancha
                let (byV2, dataType2) = iInfo2
                if (byV2 == False) && (byV == False) then
                  return ()
                else
                  liftEither $ throwError $
                  "Both side of this assignment must pass by reference "
            else
              return ()

        else
          return ()
-- check read
-- 1.Check if lvalue uses the correct format
-- 2.Check lvalue is Boolean or Integer type
checkStmt (Read lvalue)
  =
    do
      checkLValue lvalue
      lvalueInfo <- getDataTypeOfLValue lvalue
      let (byV,lvalueType) = lvalueInfo
      if (lvalueType == BaseDataType BooleanType)
        || (lvalueType == BaseDataType IntegerType) then
        return ()
      else
        liftEither $ throwError
        ("Read lvalue,lvalue is not a boolean or integer type lvalue")
-- check write
-- 1.Check if exp uses the correct format
-- 2.Check if exp is Boolean Integer or String literal
checkStmt (Write exp)
  =
    do
      checkExp exp
      expType<-getExpType exp
      if (expType == BaseDataType BooleanType)
        || (expType == BaseDataType IntegerType)
        || (expType == BaseDataType StringType) then
        return ()
      else
        liftEither $ throwError
        ("write exp, exp is not a boolean or integer, or a string literal.")
-- Check writeln: same as write
checkStmt (Writeln exp)
  =
    do
      checkExp exp
      expType <- getExpType exp
      if (expType == BaseDataType BooleanType)
        || (expType == BaseDataType IntegerType)
        || (expType == BaseDataType StringType) then
        return ()
      else
        liftEither $ throwError
        ("writeln exp, exp is not a boolean or integer, or a string literal.")

-- Check Ifthen
-- 1.Check if exp uses the correct format
-- 2.Check if exp is Boolean type
-- 3.check stmts
checkStmt (IfThen exp stmts)
  =
    do
      checkExp exp
      expType <- getExpType exp
      if not (expType == (BaseDataType BooleanType)) then
          liftEither $ throwError ("IfThen exp, exp is not boolean type")
      else
        do
          checkStmts stmts
-- Check IfThenElse
-- 1.Check if exp uses the correct format
-- 2.Check if exp is Boolean type
-- 3.check two "stmts"
checkStmt (IfThenElse exp stmts1 stmts2)
  =
    do
      checkExp exp
      expType <- getExpType exp
      if not (expType == (BaseDataType BooleanType)) then
          liftEither $ throwError
          ("IfThenElse exp,exp is not boolean type")
      else
        do
          checkStmts stmts1
          checkStmts stmts2
-- Check While
-- 1.Check if exp uses the correct format
-- 2.Check if exp is Boolean type
-- 3.check stmts
checkStmt (While exp stmts )
  =
    do
      checkExp exp
      expType <- getExpType exp
      if not (expType == (BaseDataType BooleanType)) then
          liftEither $ throwError
          ("While exp, exp is not boolean type")
      else
        do
          checkStmts stmts
-- Check Call
-- 1.Check if exps use the correct format
-- 2.Check if procedure called "procedureName" is exist
-- 3.The number of actual parameters in a call must be equal to the number
--    of formal parameters in the procedure’s definition.
-- 4.The type of actual parameters in a call must match the type of
--    formal parameters in the procedure’s definition.
checkStmt (Call procedureName exps)
  =
    do
      mapM_ checkExp exps
      (proParams, procCalled) <- getProcedure procedureName
      let nParamsFound = length exps
      let nParamsExpected = length proParams
      if nParamsFound /= nParamsExpected then
        liftEither $ throwError (show nParamsExpected ++
        " parameters expected for procedure: \"" ++ procedureName ++ "\" " ++
        show nParamsFound ++ " parameters found")
      else
        do
          temp <- hasSameElem exps proParams
          if not temp then
            liftEither $ throwError
            ("The type of the parameter does not match what you are calling")
          else
            do
              return ()


------------------------------------------------------------------------------
-----------------------Semantic checking on lvalue----------------------------
-- Check all lvalue
-- Check four kinds of lvalue
-- 1.all variable should been declared before using
-- 2.<varname>
--   <recordName>.<fieldname>
--        <recordName> should be record type,and <fieldname> should been in
--      this kind of record.
--   <arrayName>[index]
--        <arrayName> should be array type.
--        [index] this exp should be integer type.
--   <arrayName>[index].<fieldname>
--        <arrayName> should be array type storing record type,and <fieldname>
--      should been in this kind of record.
--        [index] this exp should be integer type.
--
checkLValue :: LValue -> SymTableState ()
-- <varname>
checkLValue (LId varName)
  =
    do
      cvt <- getCurVariableTable
      if (Map.member varName (vtt cvt)) then
        do
          return()
      else
        liftEither $ throwError $ "Undeclared variable name: " ++ varName

-- <recordvarname>.<fieldname>
checkLValue (LDot recordVarname fieldName)
  =
    do
      cvt <- getCurVariableTable
      if (Map.member recordVarname (vtt cvt)) then
        do
          st <- get
          c <- getVariableType recordVarname
          let (bool, int1, variableType, int2) = c
          if varIsRecordType variableType then
            do
              let (RecordVar recordType) = variableType
              let ck = CompositeKey recordType fieldName

              if (Map.member ck (rft st)) then
                 return ()
              else
                liftEither $ throwError $ "Record.field: " ++
                recordVarname ++ "." ++ fieldName
                ++ " does not exist"
          else
              liftEither $ throwError $
              recordVarname++" is not a record name"
      else
        liftEither $ throwError $
        "Undeclared variable name: " ++ recordVarname

-- <arrayVarName> [index]
checkLValue (LBrackets arrayName int)
  =
    do
      cvt <- getCurVariableTable
      if (Map.member arrayName (vtt cvt)) then
        do
          varInfo <- (getVariableType arrayName)
          let (bool,int1,vartype,arraysize) = varInfo
          if varIsArrayType vartype then
            do
                let (ArrayVar arrayType) = vartype
                artype <- getArrayType arrayType
                let (intt, dataType) = artype

                indextype <- getExpType int
                if indextype == BaseDataType IntegerType then
                  return()
                else
                  liftEither $ throwError $
                  "Array's index should be an integer type "
          else
            liftEither $ throwError $
            arrayName ++ " is not array variable name "

      else
        liftEither $ throwError $
        "Undeclared variable name: " ++ arrayName
-- <arrayVarName>[index].<fieldname>
checkLValue (LBracketsDot arrayName int fieldName)
  =
    do
      cvt <- getCurVariableTable
      if (Map.member arrayName (vtt cvt)) then
        do
          c <- getVariableType arrayName
          let (bool, int1, variableType, int2) = c
          if varIsArrayType variableType then
            do
              let (ArrayVar arrayType) = variableType
              artype <- getArrayType arrayType
              let (intt, dataType) = artype

              if dataIsRecordTypeStoreInArray dataType then
                do
                  let (AliasDataType alsName) = dataType
                  st <- get
                  let ck = CompositeKey alsName fieldName
                  if (Map.member ck (rft st)) then
                    do
                      indexType <- getExpType int
                      if indexType == BaseDataType IntegerType then
                        return()
                      else
                        liftEither $ throwError $
                        "Array's index should be an integer type "
                  else
                    liftEither $ throwError $
                    "Record.field: " ++ arrayName ++ "[]." ++ fieldName ++
                                            " does not exist"
              else
                liftEither $ throwError $ arrayName ++
                " is not a array storing record "
          else
            liftEither $ throwError $  arrayName ++
            " is not array variable name "
      else
        liftEither $ throwError $ "Undeclared variable name: " ++ arrayName

------------------------------------------------------------------------------
-----------------semantic check on all kinds of expression--------------------

checkExp :: Exp -> SymTableState ()
checkExp (BoolConst bool)
  =
    do return()

checkExp (IntConst int)
  =
    do return()

checkExp (StrConst string)
  =
    do return()

checkExp (Op_or exp exp2)
  =
    do
      checkExp exp
      checkExp exp2
      type1 <- getExpType exp
      type2 <- getExpType exp2
      if (dataIsBoolType type1) && (dataIsBoolType type2) then
        return ()
      else
        liftEither $ throwError $
        "two exps in or operation must be in boolean type"


checkExp (Op_and exp exp2)
  =
    do
      checkExp exp
      checkExp exp2
      type1 <- getExpType exp
      type2 <- getExpType exp2
      if (dataIsBoolType type1) && (dataIsBoolType type2) then
        return ()
      else
        liftEither $ throwError $
        "two exps in and operation must be in boolean type"

checkExp (Op_eq exp exp2)
  =
    do
      checkExp exp
      checkExp exp2
      type1 <- getExpType exp
      type2 <- getExpType exp2
      if (dataIsBolIntType type1) && (dataIsBolIntType type2) then
        if type1==type2 then
          return()
        else
          liftEither $ throwError $
          "two exps in eq operation must be same type"
      else
        liftEither $ throwError $
        "two exps in eq operation must be boolean or integer type"

checkExp (Op_neq exp exp2)
  =
    do
      checkExp exp
      checkExp exp2
      type1 <- getExpType exp
      type2 <- getExpType exp2
      if (dataIsBolIntType type1) && (dataIsBolIntType type2) then
        if type1 == type2 then
          return()
        else
          liftEither $ throwError $
          "two exps in neq operation must be same type"
      else
        liftEither $ throwError $
        "two exps in neq operation must be boolean or integer type"

checkExp (Op_less exp exp2)
  =
    do
      checkExp exp
      checkExp exp2
      type1 <- getExpType exp
      type2 <- getExpType exp2
      if (dataIsBolIntType type1) && (dataIsBolIntType type2) then
        if type1 == type2 then
          return()
        else
          liftEither $ throwError $
          "two exps in less operation must be same type"
      else
        liftEither $ throwError $
        "two exps in less operation must be boolean or integer type"

checkExp (Op_less_eq exp exp2)
  =
    do
      checkExp exp
      checkExp exp2
      type1 <- getExpType exp
      type2 <- getExpType exp2
      if (dataIsBolIntType type1) && (dataIsBolIntType type2) then
        if type1 == type2 then
          return()
        else
          liftEither $ throwError $
          "two exps in <= operation must be same type"
      else
        liftEither $ throwError $
        "two exps in <= operation must be boolean or integer type"

checkExp (Op_large exp exp2)
  =
    do
      checkExp exp
      checkExp exp2
      type1 <- getExpType exp
      type2 <- getExpType exp2
      if (dataIsBolIntType type1) && (dataIsBolIntType type2) then
        if type1 == type2 then
          return()
        else
          liftEither $ throwError $
          "two exps in large operation must be same type"
      else
        liftEither $ throwError $
        "two exps in large operation must be boolean or integer type"

checkExp (Op_large_eq exp exp2)
  =
    do
      checkExp exp
      checkExp exp2
      type1 <- getExpType exp
      type2 <- getExpType exp2
      if (dataIsBolIntType type1) && (dataIsBolIntType type2) then
        if type1 == type2 then
          return()
        else
          liftEither $ throwError $
          "two exps in >= operation must be same type"
      else
        liftEither $ throwError $
        "two exps in >= operation must be boolean or integer type"

checkExp (Op_add exp exp2)
  =
    do
      checkExp exp
      checkExp exp2
      type1 <- getExpType exp
      type2 <- getExpType exp2
      if (dataIsIntegerType type1) && (dataIsIntegerType type2) then
        return ()
      else
        liftEither $ throwError $
        "two exps in add operation must be in integer type"

checkExp (Op_sub exp exp2)
  =
    do
      checkExp exp
      checkExp exp2
      type1 <- getExpType exp
      type2 <- getExpType exp2
      if (dataIsIntegerType type1) && (dataIsIntegerType type2) then
        return ()
      else
        liftEither $ throwError $
        "two exps in sub operation must be in integer type"

checkExp (Op_mul exp exp2)
  =
    do
      checkExp exp
      checkExp exp2
      type1 <- getExpType exp
      type2 <- getExpType exp2
      if (dataIsIntegerType type1) && (dataIsIntegerType type2) then
        return ()
      else
        liftEither $ throwError $
        "two exps in mul operation must be in integer type"

checkExp (Op_div exp exp2)
  =
    do
      checkExp exp
      checkExp exp2
      type1 <- getExpType exp
      type2 <- getExpType exp2
      if (dataIsIntegerType type1)&&(dataIsIntegerType type2) then
        return ()
      else
        liftEither $ throwError $
         "two exps in div operation must be in integer type"

checkExp (Op_not exp )
  =
    do
      checkExp exp
      type1 <- getExpType exp
      if (dataIsBoolType type1) then
        return ()
      else
        liftEither $ throwError $
        " exp after not operation must be in boolean type"

checkExp (Op_neg exp)
  =
    do
      checkExp exp
      type1 <- getExpType exp
      if (dataIsIntegerType type1) then
        return ()
      else
        liftEither $ throwError $
        "exp after neg operation must be in integer type"

checkExp (Lval lvalue)
  =
    do
      checkLValue lvalue


------------------------------------------------------------------------------
---------------------------- help functions-----------------------------------

------------------------------------------------------------------------------
-- check arity of a procedure
checkArityProcedure :: String -> Int -> SymTableState ()
checkArityProcedure procedureName arity
  =
    do
      st <- get
      let (procedureParams, _) = (pt st) Map.! procedureName
      let procedureArity = length $ procedureParams
      if procedureArity == arity then
        return ()
      else
        liftEither $ throwError ("Unmatched arity(" ++ (show arity) ++
                                 ") for procedure: \"" ++ procedureName ++
                                 "\" found arity = " ++ (show procedureArity)
                                )


-- insert records arraies and procedures into symbol table
constructSymbolTable :: Program -> SymTableState ()
constructSymbolTable prog@(Program records arraies procedures)
  =
    do
      st <- get
      mapM_ insertRecordType records
      mapM_ insertArrayType arraies
      mapM_ insertProcedure procedures


-- function name tell us everyting

expIsLvalue :: Exp -> Bool
expIsLvalue (Lval lValue) = True
expIsLvalue _ = False

lvalueIsRerAry :: LValue ->  SymTableState Bool
lvalueIsRerAry (LId ident)
  =
    do
      varInfo <- getVariableType ident
      let (bool,int,vt,int2) = varInfo
      case vt of
        RecordVar string1 ->
          do return True
        ArrayVar string2 ->
          do return True
        _ ->
          do return False
lvalueIsRerAry (LBrackets ident exp)
  =
    do
      varInfo <- getVariableType ident
      let (bool,int,vt,int2) = varInfo
      case vt of
        ArrayVar string2 ->
          do
            arrayInfo <- getArrayType string2
            let (a,arrayType) = arrayInfo
            case arrayType of
              AliasDataType aliasType ->
                do return True
              _ ->
                do return False
        _ ->
          do return False

lvalueIsRerAry _ = do return False

varIsArrayType :: VariableType -> Bool
varIsArrayType (ArrayVar _) = True
varIsArrayType _ = False

varIsRecordType :: VariableType -> Bool
varIsRecordType (RecordVar _) = True
varIsRecordType _ = False

dataIsIntegerType :: DataType -> Bool
dataIsIntegerType (BaseDataType IntegerType) = True
dataIsIntegerType _ = False

dataIsBolIntType :: DataType -> Bool
dataIsBolIntType (BaseDataType IntegerType) = True
dataIsBolIntType (BaseDataType BooleanType) = True
dataIsBolIntType _=False

dataIsBoolType :: DataType -> Bool
dataIsBoolType (BaseDataType BooleanType) = True
dataIsBoolType _ = False

dataIsRecordTypeStoreInArray :: DataType -> Bool
dataIsRecordTypeStoreInArray (BaseDataType _) = False
dataIsRecordTypeStoreInArray (AliasDataType _) = True



--call procedure exps : all parameter type match
hasSameElem :: [Exp] -> [(Bool, DataType)] -> SymTableState Bool
hasSameElem (x:xs) ((_,y):ys)
  =
    do
      expType<-getExpType x
      if expType == y then
        hasSameElem xs ys
      else
        return False
hasSameElem [] [] = do return True
hasSameElem _ _ = do return True


------------------------------------------------------------------------------
--get expression's type
getExpType :: Exp -> SymTableState DataType

getExpType (BoolConst _) = do return (BaseDataType BooleanType)
getExpType (IntConst _) = do return (BaseDataType IntegerType)
getExpType (StrConst _) = do return (BaseDataType StringType)
getExpType (Op_or _ _) = do return (BaseDataType BooleanType)
getExpType (Op_and _ _) = do return (BaseDataType BooleanType)
getExpType (Op_eq  _ _) = do return (BaseDataType BooleanType)
getExpType (Op_neq  _ _) = do return (BaseDataType BooleanType)
getExpType (Op_less  _ _) = do return (BaseDataType BooleanType)
getExpType (Op_less_eq  _ _) = do return (BaseDataType BooleanType)
getExpType (Op_large _ _) = do return (BaseDataType BooleanType)
getExpType (Op_large_eq _ _) = do return (BaseDataType BooleanType)
getExpType (Op_not _) = do return (BaseDataType BooleanType)
getExpType (Op_add _ _) = do return (BaseDataType IntegerType)
getExpType (Op_sub _ _) = do return (BaseDataType IntegerType)
getExpType (Op_mul _ _) = do return (BaseDataType IntegerType)
getExpType (Op_div _ _) = do return (BaseDataType IntegerType)
getExpType (Op_neg _) = do return (BaseDataType IntegerType)
getExpType (Lval lvalue )
  =
    do
      dataInfo <- getDataTypeOfLValue lvalue
      let (byV,dataType) = dataInfo
      return dataType

--get the data type of a lvalue
--this should be used after a checkvalue

getDataTypeOfLValue :: LValue -> SymTableState (Bool,DataType)
-- <id>
getDataTypeOfLValue (LId varname)
  =
    do
      varInfo <- getVariableType varname
      let (bool,int,vt,int2) = varInfo
      let datatype = (varTypeToDataType vt) in return (bool,datatype)

-- <id>.<id>
getDataTypeOfLValue (LDot recordName fieldname)
  =
    do
      c <- getVariableType recordName
      let (bool, int1, variableType, int2) = c
      let (RecordVar recordType) = variableType
      b <- getRecordField recordType fieldname
      let datatype = fst b in return (bool,(BaseDataType datatype))


-- <id> [Int]
getDataTypeOfLValue (LBrackets arrayName int)
  =
    do
      c <- getVariableType arrayName
      let (bool, int, variableType, int2) = c
      let (ArrayVar arrayType) = variableType
      a <- getArrayType arrayType
      let datatype =snd a in return (bool,datatype)
-- <id> [Int]. <id>
getDataTypeOfLValue (LBracketsDot arrayName int fieldname)
  =
    do
       c<-getVariableType arrayName
       let (bool, int, variableType, int2) = c
       let (ArrayVar arrayType) = variableType
       a <- getArrayType arrayType
       let AliasDataType recordName = snd a

       b <- getRecordField recordName fieldname
       let datatype = fst b in return (bool,(BaseDataType datatype))


-- variable type-> data type
varTypeToDataType :: VariableType -> DataType
varTypeToDataType (BooleanVar) = BaseDataType BooleanType
varTypeToDataType (IntegerVar) = BaseDataType IntegerType
varTypeToDataType (RecordVar alias) = AliasDataType alias
varTypeToDataType (ArrayVar alias) = AliasDataType alias

-- used to report error
getLValueName :: LValue -> String
getLValueName (LId ident) = ident
getLValueName (LDot ident ident2) = ident
getLValueName (LBrackets ident exp ) = ident
getLValueName (LBracketsDot ident exp ident2) = ident

-- used to report error
getDataT :: DataType -> String
getDataT (BaseDataType BooleanType) = "Boolean"
getDataT (BaseDataType IntegerType) = "Integer"
getDataT (BaseDataType StringType) = "String"
getDataT (AliasDataType aliasType ) = aliasType
