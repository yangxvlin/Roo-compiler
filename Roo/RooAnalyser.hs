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
      (_, mainProc@(Procedure _ (ProcedureBody _ mainProcStmts))) <- 
        getProcedure "main"
      checkArityProcedure "main" 0

      st <- get
      let procedures = pt st
      mapM_ checkOneProcedures  procedures
      st2 <- get
      return st2

--------------------------------------------------------------------------------
---------------------Semantic checking on a procedures------------------------
-- checkOneProcedures
-- 1.initialize local variable table
-- 2.insert procedure's parameter and variable into lvt
-- 3.check all statements
-- 4.Empty lvt

checkOneProcedures:: ([(Bool, DataType)], Procedure)->SymTableState ()
checkOneProcedures  (_, procCalled@(Procedure _ (ProcedureBody _ procStmts)))
  =
    do
        pushLocalVariableTable        
        insertProcedureVariable procCalled 
        checkStmts procStmts
        popLocalVariableTable

--------------------------------------------------------------------------------
------------Semantic checking on all statement of a procedure-----------------
checkStmts :: [Stmt] -> SymTableState ()
checkStmts = mapM_ checkStmt

--check one procedure:
checkStmt :: Stmt -> SymTableState ()
-- check assign:
-- 1.Check if lvalue and exp use the correct format
-- 2.check if lvalue and exp have same data type
checkStmt (Assign lvalue exp) 
  = 
    do  
      checkLValue lvalue
      checkExp exp
      identi <- getDatatypeoflvalue lvalue
      expType <- getExpType exp
      let showLValueName = getLValueName lvalue 
      let showTypeName = getDataT expType
                 
      if  not (identi == expType) then
        liftEither $ throwError $ "assign a wrong type "
        ++ showTypeName ++ " to " ++ showLValueName 
      else
        return ()
-- check read
-- 1.Check if lvalue uses the correct format 
-- 2.Check lvalue is Boolean or Integer type
checkStmt (Read lvalue) 
  = 
    do
      checkLValue lvalue
      lvalueType <- getDatatypeoflvalue lvalue
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
      exptype<-getExpType exp
      if not (exptype==(BaseDataType BooleanType))then
          liftEither $ throwError ("IfThenElse exp,exp is not boolean type")
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
      exptype<-getExpType exp
      if not (exptype==(BaseDataType BooleanType))then
          liftEither $ throwError ("While exp, exp is not boolean type")
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
          temp<-hasSameElem exps proParams
          if not temp 
          then 
            liftEither $ throwError 
            ("The type of the parameter does not match what you are calling")
          else 
            do
              return ()

          
--------------------------------------------------------------------------------
-----------------------Semantic checking on lvalue----------------------------
-- Check all lvalue
-- Check four kinds of lvalue
-- 1.all variable should been declared before using
-- 2.<varname>    <varname> should be boolean or integer type.
--   <recordname>.<fieldname>
--        <recordname> should be record type,and <fieldname> should been in this 
--      kind of record.
--   <arrayname>[index]
--        <arrayname> should be array type.
--        [index] this exp should be integer type.
--   <arrayname>[index].<fieldname>
--        <arrayname> should be array type storing record type,and <fieldname> 
--      should been in this kind of record.
--        [index] this exp should be integer type.
--   
checkLValue::LValue->SymTableState ()
-- <varname>
checkLValue (LId varName) 
  =
    do
      cvt <- getCurVariableTable
      if (Map.member varName (vtt cvt)) then
        do
          return()
          -- varInfo <- (getVariableType varName)
          -- let (bool,int1,vartype,int2)=varInfo
          -- if isnotRcdAryType vartype then
          --  return()
          -- else
          --   liftEither $ throwError $
          --   varName++" cannot be Record name or array name: "  

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
          c<-getVariableType recordVarname
          let (bool, int1, variableType, int2)=c
          if varisRecordType variableType then 
            do

              let (RecordVar recordType)=variableType
              let ck = CompositeKey recordType fieldName
           
      -- get a (record name, field name) definition
              if (Map.member ck (rft st)) then
                 return () 
      -- no (record name, field name) definition
              else
                liftEither $ throwError $ "Record.field: " ++ 
                                      recordVarname ++ "." ++ fieldName ++ 
                                      " does not exist"
          else
              liftEither $ throwError $ recordVarname++" is not a record name"
      else 
        liftEither $ throwError $ "Undeclared variable name: " ++ recordVarname

-- <arrayVarName> [index]
checkLValue (LBrackets arrayName int) 
  =
    do
      cvt <- getCurVariableTable
      if (Map.member arrayName (vtt cvt)) then
        do
          varInfo <- (getVariableType arrayName)
          let (bool,int1,vartype,arraysize)=varInfo
          if varisArrayType vartype then
            do
                let (ArrayVar arrayType)=vartype
                artype<-getArrayType arrayType
                let (intt, dataType)=artype
                --if not (dataisRecordtypeStoreinary dataType) then
              --    do
                indextype<-getExpType int
                if indextype==BaseDataType IntegerType then
                  return()
                else
                  liftEither $ throwError $ 
                  "Array's index should be an integer type " 
                ---else
              --    liftEither $ throwError $  
               --   arrayName++" is not array storing integer or boolean "


          else
            liftEither $ throwError $  
            arrayName++" is not array variable name "

      else 
        liftEither $ throwError $ "Undeclared variable name: " ++ arrayName
-- <arrayVarName>[index].<fieldname>
checkLValue (LBracketsDot arrayName int fieldName) 
  =
    do
      cvt <- getCurVariableTable
      if (Map.member arrayName (vtt cvt)) then
        do
          c<-getVariableType arrayName
          let (bool, int1, variableType, int2)=c
          if varisArrayType variableType then
            do
              let (ArrayVar arrayType)=variableType
              artype<-getArrayType arrayType
              let (intt, dataType)=artype
              
              if dataisRecordtypeStoreinary dataType then
                do
                  let (AliasDataType alsName)=dataType
                  st <- get
                  let ck = CompositeKey alsName fieldName
                  if (Map.member ck (rft st)) then
                    do
                      indextype<-getExpType int
                      if indextype==BaseDataType IntegerType then
                        return()
                      else
                        liftEither $ throwError $ 
                        "Array's index should be an integer type " 
              
                  else
                    liftEither $ throwError $ 
                    "Record.field: " ++ arrayName ++ "[]." ++ fieldName ++ 
                                            " does not exist"
              else
                liftEither $ throwError $ arrayName++
                " is not a array storing record " 
          else
            liftEither $ throwError $  arrayName++
            " is not array variable name "
      else 
        liftEither $ throwError $ "Undeclared variable name: " ++ arrayName
      
--------------------------------------------------------------------------------
-----------------semantic check on all kinds of expression----------------------

checkExp::Exp->SymTableState ()
checkExp (BoolConst bool)
  =do return()

checkExp (IntConst int)
  =do return()

checkExp (StrConst string)
  =do return()

checkExp (Op_or exp exp2)
  =do 
    checkExp exp 
    checkExp exp2
    type1 <- getExpType exp
    type2 <- getExpType exp2
    if (dataisBoolType type1)&&(dataisBoolType type2) then
      return ()
    else
      liftEither $ throwError $ 
      "two exps in or operation must be in boolean type"


checkExp (Op_and exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType exp
  type2 <- getExpType exp2
  if (dataisBoolType type1)&&(dataisBoolType type2) then
    return ()
  else
    liftEither $ throwError $ 
    "two exps in and operation must be in boolean type"

checkExp (Op_eq exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType exp
  type2 <- getExpType exp2
  if (dataisBolIntType type1)&&(dataisBolIntType type2) then
    if type1==type2 then
      return()
    else
      liftEither $ throwError $ 
      "two exps in eq operation must be same type"
  else
    liftEither $ throwError $ 
    "two exps in eq operation must be boolean or integer type"
  
checkExp (Op_neq exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType exp
  type2 <- getExpType exp2
  if (dataisBolIntType type1)&&(dataisBolIntType type2) then
    if type1==type2 then
      return()
    else
      liftEither $ throwError $ 
      "two exps in neq operation must be same type"
  else
    liftEither $ throwError $ 
    "two exps in neq operation must be boolean or integer type"
  
checkExp (Op_less exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType exp
  type2 <- getExpType exp2
  if (dataisBolIntType type1)&&(dataisBolIntType type2) then
    if type1==type2 then
      return()
    else
      liftEither $ throwError $ "two exps in less operation must be same type"
  else
    liftEither $ throwError $ 
    "two exps in less operation must be boolean or integer type"
    
checkExp (Op_less_eq exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType exp
  type2 <- getExpType exp2
  if (dataisBolIntType type1)&&(dataisBolIntType type2) then
    if type1==type2 then
      return()
    else
      liftEither $ throwError $ "two exps in <= operation must be same type"
  else
    liftEither $ throwError $ 
    "two exps in <= operation must be boolean or integer type"

checkExp (Op_large exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType exp
  type2 <- getExpType exp2
  if (dataisBolIntType type1)&&(dataisBolIntType type2) then
    if type1==type2 then
      return()
    else
      liftEither $ throwError $ "two exps in large operation must be same type"
  else
    liftEither $ throwError $ 
    "two exps in large operation must be boolean or integer type"

checkExp (Op_large_eq exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType exp
  type2 <- getExpType exp2
  if (dataisBolIntType type1)&&(dataisBolIntType type2) then
    if type1==type2 then
      return()
    else
      liftEither $ throwError $ "two exps in >= operation must be same type"
  else
    liftEither $ throwError $ 
    "two exps in >= operation must be boolean or integer type"

checkExp (Op_add exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType exp
  type2 <- getExpType exp2
  if (dataisIntegerType type1)&&(dataisIntegerType type2) then
    return ()
  else
    liftEither $ throwError $ "two exps in add operation must be in integer type"
    
checkExp (Op_sub exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType exp
  type2 <- getExpType exp2
  if (dataisIntegerType type1)&&(dataisIntegerType type2) then
    return ()
  else
    liftEither $ throwError $ 
    "two exps in sub operation must be in integer type"
      
checkExp (Op_mul exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType exp
  type2 <- getExpType exp2
  if (dataisIntegerType type1)&&(dataisIntegerType type2) then
    return ()
  else
    liftEither $ throwError $
    "two exps in mul operation must be in integer type"
     
checkExp (Op_div exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType exp
  type2 <- getExpType exp2
  if (dataisIntegerType type1)&&(dataisIntegerType type2) then
    return ()
  else
    liftEither $ throwError $
     "two exps in div operation must be in integer type"
     
checkExp (Op_not exp )
  =do 
  checkExp exp 
  type1 <- getExpType exp
  if (dataisBoolType type1) then
    return ()
  else
    liftEither $ throwError $ 
    " exp after not operation must be in boolean type"
       
checkExp (Op_neg exp)
  =do 
  checkExp exp 
  type1 <- getExpType exp
  if (dataisIntegerType type1) then
    return ()
  else
    liftEither $ throwError $ "exp after neg operation must be in integer type"

checkExp (Lval lvalue)
  =
    do
      checkLValue lvalue
      

--------------------------------------------------------------------------------
---------------------------- help functions-------------------------------------

--------------------------------------------------------------------------------
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
varisArrayType::VariableType->Bool
varisArrayType (ArrayVar _)=True
varisArrayType _=False

varisRecordType::VariableType->Bool
varisRecordType (RecordVar _)=True
varisRecordType _=False

isnotRcdAryType::VariableType->Bool
isnotRcdAryType (RecordVar _)=False
isnotRcdAryType (ArrayVar _)=False
isnotRcdAryType _=True

jdatatype::DataType->Bool
jdatatype (BaseDataType IntegerType)=True
jdatatype (BaseDataType BooleanType)=True
jdatatype _=False

dataisIntegerType::DataType->Bool
dataisIntegerType (BaseDataType IntegerType)=True
dataisIntegerType _=False

dataisBolIntType::DataType->Bool
dataisBolIntType (BaseDataType IntegerType)=True
dataisBolIntType (BaseDataType BooleanType)=True
dataisBolIntType _=False

dataisBoolType::DataType->Bool
dataisBoolType (BaseDataType BooleanType)=True
dataisBoolType _=False

dataisRecordtypeStoreinary::DataType->Bool
dataisRecordtypeStoreinary (BaseDataType _)=False
dataisRecordtypeStoreinary (AliasDataType _)=True

    

--call procedure exps : all parameter type match
hasSameElem::[Exp]->[(Bool, DataType)]->SymTableState Bool
hasSameElem (x:xs) ((_,y):ys)
  =do
      a<-getExpType x
      if a==y then
        hasSameElem xs ys 
      else
        return False
hasSameElem [] []=do return True
hasSameElem _ _=do return True

      
--------------------------------------------------------------------------------
--get expression's type
getExpType::Exp->SymTableState DataType

getExpType (BoolConst _)= do 
  return (BaseDataType BooleanType)
getExpType (IntConst _)= do return (BaseDataType IntegerType)
getExpType (StrConst a)=do return (BaseDataType StringType)
getExpType (Op_or _ _)=do return (BaseDataType BooleanType)
getExpType (Op_and _ _)=do return (BaseDataType BooleanType)
getExpType (Op_eq  _ _)=do return (BaseDataType BooleanType)
getExpType (Op_neq  _ _)=do return (BaseDataType BooleanType)
getExpType (Op_less  _ _)=do return (BaseDataType BooleanType)
getExpType (Op_less_eq  _ _)=do return (BaseDataType BooleanType)
getExpType (Op_large _ _)=do return (BaseDataType BooleanType)
getExpType (Op_large_eq _ _)=do return (BaseDataType BooleanType)
getExpType (Op_not _)=do return (BaseDataType BooleanType)
getExpType (Op_add _ _)=do return (BaseDataType IntegerType)
getExpType (Op_sub _ _)=do return (BaseDataType IntegerType)
getExpType (Op_mul _ _)=do return (BaseDataType IntegerType)
getExpType (Op_div _ _)=do return (BaseDataType IntegerType)
getExpType (Op_neg _)=do return (BaseDataType IntegerType)
getExpType (Lval lvalue )=
  do   
    datatype<-getDatatypeoflvalue lvalue
    return datatype

--get the data type of a lvalue
--this should be used after a checkvalue

getDatatypeoflvalue::LValue->SymTableState DataType
-- <id>
getDatatypeoflvalue (LId varname) 
  = do
      varInfo <- getVariableType varname
      let (bool,int,vt,int2)=varInfo
      let datatype = (varTypetoDataType vt )in return datatype

-- <id>.<id>
getDatatypeoflvalue (LDot recordname fieldname) 
  = do
      c<-getVariableType recordname
      let (bool, int1, variableType, int2)=c
      let (RecordVar recordType)=variableType 
      b <- getRecordField recordType fieldname 
      let datatype = fst b in return (BaseDataType datatype)

      
-- <id> [Int]     
getDatatypeoflvalue (LBrackets arrayname int) 
  = do
      c<-getVariableType arrayname
      let (bool, int, variableType, int2)=c
      let (ArrayVar arrayType)=variableType
      a <- getArrayType arrayType
      let datatype =snd a in return datatype
-- <id> [Int]. <id>     
getDatatypeoflvalue (LBracketsDot arrayname int fieldname) 
  = do
       c<-getVariableType arrayname
       let (bool, int, variableType, int2)=c
       let (ArrayVar arrayType)=variableType
       a <- getArrayType arrayType
       let AliasDataType recordname =snd a
       
       b <- getRecordField recordname fieldname 
       let datatype = fst b in return (BaseDataType datatype)
       
    
--variable type-> data type
varTypetoDataType::VariableType->DataType
varTypetoDataType (BooleanVar)=BaseDataType BooleanType
varTypetoDataType (IntegerVar)=BaseDataType IntegerType
varTypetoDataType (RecordVar alias)=AliasDataType alias
varTypetoDataType (ArrayVar alias)=AliasDataType alias

--used to report error  
getLValueName::LValue->String
getLValueName (LId ident)=ident  
getLValueName (LDot ident ident2)=ident  
getLValueName (LBrackets ident exp )=ident  
getLValueName (LBracketsDot ident exp ident2)=ident 
                
--used to report error
getDataT::DataType->String
getDataT (BaseDataType BooleanType) ="Boolean"
getDataT (BaseDataType IntegerType) ="Integer"
getDataT (BaseDataType StringType) ="String"
getDataT (AliasDataType aliasType ) =aliasType

