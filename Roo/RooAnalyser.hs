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
--中文注释最后删除--chinese comments will be deleted
--main function  of semantic analysis

--TODO:

------The scope of a declared variable (or of a formal procedure parameter) is the enclosing procedure definition.
------Procedures can use (mutual) recursion.(?)
analyse :: Program -> Result 
analyse prog
  = evalStateT (semanticCheckRooProgram prog) initialSymTable

--semantic analysis on a roo program  symtable->Either string (a,symtable)
semanticCheckRooProgram :: Program -> SymTableState SymTable
semanticCheckRooProgram prog
  =
    do
      constructSymbolTable prog
      (_, mainProc@(Procedure _ (ProcedureBody _ mainProcStmts))) <- 
        getProcedure "main"
      -- Every program must contain a procedure of arity 0 named "main"
      checkArityProcedure "main" 0

      --已在symboltable处检查了procedure name不会重复，同时保证了只有一个main 
      
      --get procedure table and check stmts of all procedures in procedure table
      st <- get
      let procedures=pt st
      let procWithoutMain =delete "main" procedures 
      pushLocalVariableTable
      insertProcedureVariable mainProc
      checkStmts mainProcStmts
      popLocalVariableTable
      mapM_ (checkOneProcedures mainProc) procWithoutMain


      st2 <- get
      return st2

--------------------下面检查procedure-----------------------
------Semantic checking on all procedures------------------------
--分两部分:参数和stmt
--第一部分已经在symtable检查了每个procedure的形参localvar不重复
--第二部分逐步检查每个stmt
-- checkAllProcedures :: Map String ([(Bool, DataType)], Procedure) ->SymTableState ()
-- checkAllProcedures procedures 
--   = do
--       let procWithoutMain =delete "main" procedures 
--       pushLocalVariableTable
--       insertProcedureVariable mainProc
--       mapM_ checkOneProcedures procWithoutMain
checkOneProcedures:: Procedure->([(Bool, DataType)], Procedure)->SymTableState ()
checkOneProcedures mainProc procedure@((proParams, procCalled@(Procedure _ (ProcedureBody _ procStmts))))
  =
    do
        pushLocalVariableTable
        insertProcedureVariable mainProc 
        insertProcedureVariable procCalled 
        checkStmts procStmts
        popLocalVariableTable
      

  
-- construct global type tables 将prog中保存的三个元素存入symboltable
constructSymbolTable :: Program -> SymTableState ()
constructSymbolTable prog@(Program records arraies procedures)
  = 
    do
      st <- get
      mapM_ insertRecordType records    --att -rft
      mapM_ insertArrayType arraies     --att
      mapM_ insertProcedure procedures  --pt

-- all type aliases must be distinct, record and array has no overlapping name
-- soln: handled when insertRecordType & insertArrayType as defined in 
  --     SymbolTable.hs

-- Within a given procedure, variable names, including formal parameter names, 
-- must be distinct.
-- soln: handled when insertProcedureVariable by calling 
--       insertProcedureParameter & insertProcedureVariableDecl in 
--       SymbolTable.hs

-- procedure's parameter and local variable should have different name

-- different procedures' parameter and local variable could have same name 
-- soln: handled by each procedure's param and local var are stored in separate 
--       LocalVariableTable as defined in SymbolTable.hs

-- all defined procedures must have distinct names.
-- soln: handled when insertProcedure as defined in SymbolTable.hs



----------------------checkstmt:---------------------------------
checkStmts :: [Stmt] -> SymTableState ()
checkStmts = mapM_ checkStmt

--下面是检查所有类型的stmt，由于每个stmt都要检查所用var是否声明过，需要再写一个
--checkLValue
-- <id>

jvartype2::VariableType->Bool
jvartype2 (RecordVar _)=False
jvartype2 (ArrayVar _)=True
jvartype2 _=False

--if this array's Datatype is a record and this record has a 
--fieldname:fieldName,return True
jdtype::DataType->String->SymTableState Bool
jdtype (AliasDataType alsName) fieldName
  =
    do
      st <- get
      let ck = CompositeKey alsName fieldName
      -- get a (record name, field name) definition
      if (Map.member ck (rft st)) then
        return True 
      -- no (record name, field name) definition
      else
        return False  
jdtype _ _= do return False
      

--jdtype _="False"

      
checkLValue::LValue->SymTableState ()--不确定
checkLValue (LId varName) 
  =
    do
      cvt <- getCurVariableTable
      if (Map.member varName (vtt cvt)) then
        do
          varInfo <- (getVariableType varName)
          let (bool,int1,vartype,int2)=varInfo
--          if jvartype vartype then
          return()
--          else
 --           liftEither $ throwError $ "<id> in (lvalue <id>) cannot be Record name or array name: " ++ varName

      else 
        liftEither $ throwError $ "Undeclared variable name: " ++ varName

-- <id>.<id>   <recordvarname> <fieldname>
checkLValue (LDot recordVarname fieldName)
  =
    do
      cvt <- getCurVariableTable
      if (Map.member recordVarname (vtt cvt)) then
         do      
           st <- get
           let ck = CompositeKey recordVarname fieldName
      -- get a (record name, field name) definition
           if (Map.member ck (rft st)) then
             return () 
      -- no (record name, field name) definition
           else
              liftEither $ throwError $ "Record.field: " ++ 
                                        recordVarname ++ "." ++ fieldName ++ 
                                      " does not exist"
      else 
        liftEither $ throwError $ "Undeclared variable name: " ++ recordVarname
-- <id>[index]  <arrayVarName> [index]

checkLValue (LBrackets arrayName int) 
  =
    do
      cvt <- getCurVariableTable
      if (Map.member arrayName (vtt cvt)) then
         do
           varInfo <- (getVariableType arrayName)
           let (bool,int1,vartype,arraysize)=varInfo
           if jvartype2 vartype then
             do

               indextype<-getExpType2 int
               if indextype==BaseDataType IntegerType then
                 return()
               else
                 liftEither $ throwError $ "Array's index should be an integer type " 

           else
             liftEither $ throwError $  arrayName++" is not array variable name "

      else 
        liftEither $ throwError $ "Undeclared variable name: " ++ arrayName
-- <id>[index].<id>  <arrayVarName> [index].<fieldname>
checkLValue (LBracketsDot arrayName int fieldName) 
  =
    do
      cvt <- getCurVariableTable
      if (Map.member arrayName (vtt cvt)) then
        do
          artype<-getArrayType arrayName
          let (intt, dataType)=artype
          isRecordArray<-jdtype dataType fieldName 
          if isRecordArray then
            do
              indextype<-getExpType2 int
              if indextype==BaseDataType IntegerType then
                return()
              else
                liftEither $ throwError $ "Array's index should be an integer type " 
              
          else
            liftEither $ throwError $ "This array of record is not exist"

      else 
        liftEither $ throwError $ "Undeclared variable name: " ++ arrayName

     
      
--还需要写一个检查每个类型是否正确,需要在stmt中检查每一种Expression,
--array[e] e为正整数
--
-- The type rules for statements are as follows:
-- • In assignment statements, an lvalue on the left-hand side must have the same type t as
-- the expression on the right-hand side. If t is a record or array type, then the types of the
-- two sides must have been provided as identical type aliases, and both must have reference
-- mode. -----(handled?)
-- • Conditions in if and while statements must be of type boolean. --Their bodies must be
-- well-typed sequences of statements.(handled)
-- • For each procedure call, the number of arguments must agree with the number of formal
-- parameters used in the procedure definition, and the type of each actual parameter must
-- be the type of the corresponding formal parameter.(handled)


----------------------Get the data type that can be assigned to lvalue-------------
--get third element from state vartype
-- getthird::(Bool, Int, VariableType, Int)->VariableType
-- getthird (_,_,x,_)=x

-- <id> cannot be <recordname> or <arrayname>
jvartype::VariableType->Bool
jvartype (RecordVar _)=False
jvartype (ArrayVar _)=False
jvartype _=True
----still has bugs!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
getDatatypeoflvalue::LValue->SymTableState DataType
-- <id>
getDatatypeoflvalue (LId varname) 
  = do
      varInfo <- getVariableType varname
      let (bool,int,vt,int2)=varInfo
 --     if jvartype vt then
      let datatype = (whatVartypeNeed vt )in return datatype
  --    else
 --       liftEither $ throwError ("can not assign value to <recordname> or <arrayname>")
-- <id>.<id>
getDatatypeoflvalue (LDot recordname fieldname) 
  = do 
      b <- getRecordField recordname fieldname 
      let datatype = fst b in return (BaseDataType datatype)

      
-- <id> [Int]     
getDatatypeoflvalue (LBrackets arrayname int) 
  = do
      a <- getArrayType arrayname
      let datatype =snd a in return datatype
-- <id> [Int]. <id>     
getDatatypeoflvalue (LBracketsDot arrayname int fieldname) 
  = do
       a <- getArrayType arrayname
       let AliasDataType recordname =snd a
       b <- getRecordField recordname fieldname 
       let datatype = fst b in return (BaseDataType datatype)
       
    

whatVartypeNeed::VariableType->DataType
whatVartypeNeed (BooleanVar)=BaseDataType BooleanType
whatVartypeNeed (IntegerVar)=BaseDataType IntegerType
whatVartypeNeed (RecordVar alias)=AliasDataType alias
whatVartypeNeed (ArrayVar alias)=AliasDataType alias

  
getLValueName::LValue->String
getLValueName (LId ident)=ident  
getLValueName (LDot ident ident2)=ident  
getLValueName (LBrackets ident exp )=ident  
getLValueName (LBracketsDot ident exp ident2)=ident                 
------------------------------------------------------------------------------------
getDataT::DataType->String
getDataT (BaseDataType BooleanType) ="Boolean"
getDataT (BaseDataType IntegerType) ="Integer"
getDataT (BaseDataType StringType) ="String"
getDataT (AliasDataType aliasType ) ="Alias"


checkStmt :: Stmt -> SymTableState ()
--1.A variable must be declared (exactly once) before it is used.

--2.checkAssignType is use to check assign type:
--Boolean i----i<-0 wrong
checkStmt (Assign lvalue exp) 
  = 
    do
--      return ()
      let a=getLValueName lvalue
      checkLValue lvalue
      checkExp exp
      identi <- getDatatypeoflvalue lvalue
      exptype<- getExpType2 exp 
      let b=getDataT exptype
                 
      if  not (identi==exptype) then
 --     if not (variableType==(getExpType exp)) then
        liftEither $ throwError $ "assign a wrong type "++b++" to "++a --TODO
      else
        return ()

--write and read:
-- • The argument to read must be an lvalue of type boolean or integer.(handled)
-- • The argument to write must be a well-typed expression of type boolean or integer, or
-- a string literal. The same goes for writeln.(handled)
--  write prints integer and boolean expressions to stdout in their standard syntactic forms, 
--with no additional whitespace or newlines.
--  If write is given a string, it prints out the characters of the string to stdout, with \" 
--resulting in a double quote being printed, \n in a newline character being printed, and \t 
--in a tab being printed.
--  writeln behaves exactly like write, except it prints an additional, final newline character.
--  Similarly, read reads an integer or boolean literal from stdin and assigns it to an lvalue.
--If the user input is not valid, execution terminates.
checkStmt (Read lvalue) 
  = 
    do
      checkLValue lvalue
      lvaluetype <-getDatatypeoflvalue lvalue
      if (lvaluetype==BaseDataType BooleanType)||(lvaluetype==BaseDataType IntegerType)then        
        return ()
      else
        liftEither $ throwError ("The argument to read must be an lvalue of type boolean or integer") 


checkStmt (Write exp) 
  = 
    do
      checkExp exp
      exptype<-getExpType2 exp
      if (exptype==BaseDataType BooleanType)||(exptype==BaseDataType IntegerType)||(exptype==BaseDataType StringType)then        
        return ()
      else
        liftEither $ throwError ("The argument to write must be a well-typed expression of type boolean or integer, or a string literal.")
      

checkStmt (Writeln exp) 
  = 
    do
      checkExp exp
      exptype<-getExpType2 exp
      if (exptype==BaseDataType BooleanType)||(exptype==BaseDataType IntegerType)||(exptype==BaseDataType StringType)then        
        return ()
      else
        liftEither $ throwError ("The argument to writeln must be a well-typed expression of type boolean or integer, or a string literal.") 

checkStmt (IfThen exp stmts) 
  = 
    do
      checkExp exp
      exptype<-getExpType2 exp
      if not (exptype==(BaseDataType BooleanType))then
          liftEither $ throwError ("IF exp is not boolean type")
      else
        do
          checkStmts stmts
  
checkStmt (IfThenElse exp stmts1 stmts2) 
  = 
    do
      checkExp exp
      exptype<-getExpType2 exp
      if not (exptype==(BaseDataType BooleanType))then
          liftEither $ throwError ("IF exp is not boolean type")
      else
        do
          checkStmts stmts1
          checkStmts stmts2

checkStmt (While exp stmts ) 
  = 
    do
      checkExp exp
      exptype<-getExpType2 exp
      if not (exptype==(BaseDataType BooleanType))then
          liftEither $ throwError ("IF exp is not boolean type")
      else
        do
          checkStmts stmts
                            
------------------------call-------------------------------- 

checkStmt (Call procedureName exps) 
  = 
    do
      mapM_ checkExp exps
      --getProcedure will check if the procedure is exist
      (proParams, procCalled) <- getProcedure procedureName
      let nParamsFound = length exps
      let nParamsExpected = length proParams
      -- the number of actual parameters in a call must be equal to the number 
      -- of formal parameters in the procedure’s definition.
      if nParamsFound /= nParamsExpected then 
        liftEither $ throwError (show nParamsExpected ++ 
        " parameters expected for procedure: \"" ++ procedureName ++ "\" " ++ 
        show nParamsFound ++ " parameters found")
      else
        do
          temp<-hasSameElem exps proParams
          if not temp 
          then liftEither $ throwError ("parameters' types are not matched with what you call")
          else 
            do
              return ()
              -- TODO check type matche
          -- keeps checking the proedure being called
          --不应该call一次检查一次stmt，只用检查call的名字和参数就行
            --  pushLocalVariableTable
            --  insertProcedureVariable procCalled
            --  let (Procedure _ (ProcedureBody _ procCalledStmts)) = procCalled
            --  checkStmts procCalledStmts
            --  popLocalVariableTable
          
          

-- checkStmt _ = return ()

----TODO 补充完整
getExpType2::Exp->SymTableState DataType

getExpType2 (BoolConst _)= do 
  return (BaseDataType BooleanType)
getExpType2 (IntConst _)= do return (BaseDataType IntegerType)
getExpType2 (StrConst a)=do return (BaseDataType StringType)
getExpType2 (Op_or _ _)=do return (BaseDataType BooleanType)
getExpType2 (Op_and _ _)=do return (BaseDataType BooleanType)
getExpType2 (Op_eq  _ _)=do return (BaseDataType BooleanType)
getExpType2 (Op_neq  _ _)=do return (BaseDataType BooleanType)
getExpType2 (Op_less  _ _)=do return (BaseDataType BooleanType)
getExpType2 (Op_less_eq  _ _)=do return (BaseDataType BooleanType)
getExpType2 (Op_large _ _)=do return (BaseDataType BooleanType)
getExpType2 (Op_large_eq _ _)=do return (BaseDataType BooleanType)
getExpType2 (Op_not _)=do return (BaseDataType BooleanType)
getExpType2 (Op_add _ _)=do return (BaseDataType IntegerType)
getExpType2 (Op_sub _ _)=do return (BaseDataType IntegerType)
getExpType2 (Op_mul _ _)=do return (BaseDataType IntegerType)
getExpType2 (Op_div _ _)=do return (BaseDataType IntegerType)
getExpType2 (Op_neg _)=do return (BaseDataType IntegerType)

getExpType2 (Lval lvalue )=getDatatypeoflvalue lvalue



-- changeForm::SymTableState DataType->DataType
-- changeForm a
--   =do
--      b <- a
--      return b


-- getExpType::Exp->DataType

-- getExpType (BoolConst _)=BaseDataType BooleanType
-- getExpType (IntConst _)=BaseDataType IntegerType
-- getExpType (StrConst a)=AliasDataType a--TODO
-- getExpType (Op_or _ _)=BaseDataType BooleanType
-- getExpType (Op_and _ _)=BaseDataType BooleanType
-- getExpType (Op_eq  _ _)=BaseDataType BooleanType
-- getExpType (Op_neq  _ _)=BaseDataType BooleanType
-- getExpType (Op_less  _ _)=BaseDataType BooleanType
-- getExpType (Op_less_eq  _ _)=BaseDataType BooleanType
-- getExpType (Op_large _ _)=BaseDataType BooleanType
-- getExpType (Op_large_eq _ _)=BaseDataType BooleanType
-- getExpType (Op_not _)=BaseDataType BooleanType
-- getExpType (Op_add _ _)=BaseDataType IntegerType
-- getExpType (Op_sub _ _)=BaseDataType IntegerType
-- getExpType (Op_mul _ _)=BaseDataType IntegerType
-- getExpType (Op_div _ _)=BaseDataType IntegerType
-- getExpType (Op_neg _)=BaseDataType IntegerType

-- getExpType (Lval _ )=BaseDataType BooleanType
---sDTtoDT::SymTableState DataType->DataType
--sDTtoDT y@(SymTableState x) =x

--call exp(parameter)==procedure table parameter   
hasSameElem::[Exp]->[(Bool, DataType)]->SymTableState Bool
hasSameElem (x:xs) ((_,y):ys)
  =do
     a<-getExpType2 x
     if a==y then
       hasSameElem xs ys 
     else
       return False

hasSameElem [] []=do return True
hasSameElem _ _=do return True

-- hasSameElem::[Exp]->[(Bool, DataType)]->Bool
-- hasSameElem (x:xs) ((_,y):ys)=
--   if (getExpType x) == y 
--   then hasSameElem xs ys 
--   else False
-- hasSameElem [] []=True
-- hasSameElem _ _=True
-----------semantic check on all kinds of expression----
jexpdatatype::DataType->Bool
jexpdatatype (BaseDataType IntegerType)=True
jexpdatatype _=False

jexpdatatype2::DataType->Bool
jexpdatatype2 (BaseDataType BooleanType)=True
jexpdatatype2 _=False
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
    type1 <- getExpType2 exp
    type2 <- getExpType2 exp2
    if (jexpdatatype2 type1)&&(jexpdatatype2 type2) then
      return ()
    else
      liftEither $ throwError $ "two exps in or operation must be in boolean type"


checkExp (Op_and exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType2 exp
  type2 <- getExpType2 exp2
  if (jexpdatatype2 type1)&&(jexpdatatype2 type2) then
    return ()
  else
    liftEither $ throwError $ "two exps in and operation must be in boolean type"
-- ?
checkExp (Op_eq exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType2 exp
  type2 <- getExpType2 exp2
  if (jexpdatatype type1)&&(jexpdatatype type2) then
    return ()
  else
    liftEither $ throwError $ "two exps in eq operation must be in integer type"
  
checkExp (Op_neq exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType2 exp
  type2 <- getExpType2 exp2
  if (jexpdatatype type1)&&(jexpdatatype type2) then
    return ()
  else
    liftEither $ throwError $ "two exps in neq operation must be in integer type"
  
checkExp (Op_less exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType2 exp
  type2 <- getExpType2 exp2
  if (jexpdatatype type1)&&(jexpdatatype type2) then
    return ()
  else
    liftEither $ throwError $ "two exps in < operation must be in integer type"
    
checkExp (Op_less_eq exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType2 exp
  type2 <- getExpType2 exp2
  if (jexpdatatype type1)&&(jexpdatatype type2) then
    return ()
  else
    liftEither $ throwError $ "two exps in <= operation must be in integer type"
    
checkExp (Op_add exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType2 exp
  type2 <- getExpType2 exp2
  if (jexpdatatype type1)&&(jexpdatatype type2) then
    return ()
  else
    liftEither $ throwError $ "two exps in add operation must be in integer type"
    
checkExp (Op_sub exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType2 exp
  type2 <- getExpType2 exp2
  if (jexpdatatype type1)&&(jexpdatatype type2) then
    return ()
  else
    liftEither $ throwError $ "two exps in sub operation must be in integer type"
      
checkExp (Op_mul exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType2 exp
  type2 <- getExpType2 exp2
  if (jexpdatatype type1)&&(jexpdatatype type2) then
    return ()
  else
    liftEither $ throwError $ "two exps in mul operation must be in integer type"
     
checkExp (Op_div exp exp2)
  =do 
  checkExp exp 
  checkExp exp2
  type1 <- getExpType2 exp
  type2 <- getExpType2 exp2
  if (jexpdatatype type1)&&(jexpdatatype type2) then
    return ()
  else
    liftEither $ throwError $ "two exps in div operation must be in integer type"
     
checkExp (Op_not exp )
  =do 
  checkExp exp 
  
  type1 <- getExpType2 exp
  
  if (jexpdatatype2 type1) then
    return ()
  else
    liftEither $ throwError $ " exp after not operation must be in boolean type"
       
checkExp (Op_neg exp)
  =do 
  checkExp exp 
  
  type1 <- getExpType2 exp
  
  if (jexpdatatype type1) then
    return ()
  else
    liftEither $ throwError $ "exp after neg operation must be in integer type"

checkExp (Lval lvalue)
  =
    do
      checkLValue lvalue
      







-- data LValue 
--   = LId Ident                     -- <id>
--   | LDot Ident Ident              -- <id>.<id>
--   | LBrackets Ident Exp           -- <id>[<exp>]
--   | LBracketsDot Ident Exp Ident  -- <id>[<exp>].<id>
--     deriving (Show, Eq)

-----------semantic check on all operation------
--The language is statically typed, that is, each variable and parameter has a fixed type, chosen
--by the programmer. The type rules for expressions are as follows:
-- • The type of a Boolean constant is boolean.
-- • The type of an integer constant is integer.
-- • The type of a string literal is string.
-- -- -- • The type of an expression id is the variable id’s declared type. If the declaration uses a
-- -- -- type alias, the type is the one given by the type definition for that alias.
-- • For an expression lval .fname, lval must be of record type. The type of an expression
-- lval .fname is the type associated with field name fname, as given in the record type
-- associated with lval .(handled)
-- • For an expression id[e], id must be of array type, and e must have type integer. The
-- type of the expression is the array element type, as given in array type associated with id.
-- • Arguments of the logical operators must be of type boolean. The result of applying these
-- operators is of type boolean.
-- • The two operands of a relational operator must have the same primitive type, either
-- boolean or integer. The result is of type boolean.
-- • The two operands of a binary arithmetic operator must have type integer, and the result
-- is of type integer.
-- • The operand of unary minus must be of type integer, and the result type is the same





-----------------------------------------------------------
--procedure and variable
-----------------------------------------------------------
--all defined procedures must have distinct names.(handled)
--A defined procedure does not have to be called anywhere, 
--the definition of a procedure does not have to precede the (textually) first call to the procedure.


------Procedures can use (mutual) recursion.(?)
------the number of actual parameters in a call must be equal to the number of formal parameters in the procedure’s definition.(handled)


------The scope of a declared variable (or of a formal procedure parameter) is the enclosing procedure definition.


------A variable must be declared (exactly once) before it is used.(handled)

--a list of formal parameters must all be distinct(handled)
--the same variable/parameter name can be used in different procedures.(handled)

--In a record type definition, all field names must be given types boolean or integer.(handled)
--In an array type definition array [n] ..., n must be a positive integer.(已检查不等于0)
------Each reference to an array variable must include exactly one index expression.(不会在这个文件中体现)

-----------------------------------------------------------



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



-----------------------------------------------------------
--Dynamic semantics
-----------------------------------------------------------
--  Integer variables are automatically initialised to 0, and Boolean variables to false. 
--This extends to records and arrays.

--The semantics of arithmetic expressions and relations is standard:
--  the evaluation of an expression e1/e2 results in a runtime error if e2 evaluates to 0.
--  The ordering on Boolean values is defined by x ≤ y iff x is false or y is true (or 
--both these hold). As usual, x < y iff x ≤ y ∧ x != y.

--The logical operators are strict in all arguments and their arguments are evaluated from left to right.
--  Roo does not use short-circuit evaluation of Boolean expressions. 
--  For example,‘5 < 8 or 5 > 8/0’ causes a runtime error, rather than evaluating to true.



--The procedure “main” is the entry point, that is, execution of a program comes down to execution of a call to “main”
