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
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Either

type Result = Either String SymTable
--中文注释最后删除--chinese comments will be deleted
--main function  of semantic analysis

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
      checkAllProcedures (pt st)

      st <- get
      return st

--------------------下面检查procedure-----------------------
------Semantic checking on all procedures------------------------
--分两部分:参数和stmt
--第一部分已经在symtable检查了每个procedure的形参localvar不重复
--第二部分逐步检查每个stmt
checkAllProcedures :: Map String ([(Bool, DataType)], Procedure) ->SymTableState ()
checkAllProcedures procedures 
  =mapM_ checkOneProcedures procedures
checkOneProcedures:: ([(Bool, DataType)], Procedure)->SymTableState ()
checkOneProcedures procedure@((proParams, procCalled@(Procedure _ (ProcedureBody _ procStmts))))
  =
    do
      pushLocalVariableTable
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
--checkVarDecl
checkVarDecl::VariableDecl->SymTableState ()--不确定
checkVarDecl varw
  =return ()
--还需要写一个检查每个类型是否正确,需要在stmt中检查每一种Expression,
--array[e] e为正整数
--
-- The type rules for statements are as follows:
-- • In assignment statements, an lvalue on the left-hand side must have the same type t as
-- the expression on the right-hand side. If t is a record or array type, then the types of the
-- two sides must have been provided as identical type aliases, and both must have reference
-- mode.
-- • Conditions in if and while statements must be of type boolean. Their bodies must be
-- well-typed sequences of statements.
-- • For each procedure call, the number of arguments must agree with the number of formal
-- parameters used in the procedure definition, and the type of each actual parameter must
-- be the type of the corresponding formal parameter.
-- • The argument to read must be an lvalue of type boolean or integer.
-- • The argument to write must be a well-typed expression of type boolean or integer, or
-- a string literal. The same goes for writeln.

----------------------Get the data type that can be assigned to lvalue-------------
--get third element from state vartype
getthird::(Bool, Int, VariableType, Int)->VariableType
getthird (_,_,x,_)=x

-- <id> cannot be <recordname> or <arrayname>
jvartype::VariableType->Bool
jvartype (RecordVar _)=False
jvartype (ArrayVar _)=False
jvartype _=True

getDatatypeoflvalue::LValue->SymTableState DataType
-- <id>
getDatatypeoflvalue (LId varname) 
  = do
      a <- getVariableType varname
      if jvartype (getthird a)then
        let datatype = (whatVartypeNeed (getthird a))in return datatype
      else
        liftEither $ throwError ("can not assign value to <recordname> or <arrayname>")
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
------------------------------------------------------------------------------------


checkStmt :: Stmt -> SymTableState ()
--1.A variable must be declared (exactly once) before it is used.

--2.checkAssignType is use to check assign type:
--Boolean i----i<-0 wrong
checkStmt (Assign lvalue exp) 
  = 
    do
--      return ()
      identi <- getDatatypeoflvalue lvalue           
      if  identi==(getExpType exp) then
 --     if not (variableType==(getExpType exp)) then
        liftEither $ throwError ("assign a wrong type") --TODO
      else
        return ()

--write and read:
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
      return ()

checkStmt (Write exp) 
  = 
    do
      return ()

checkStmt (Writeln exp) 
  = 
    do
      return ()  

checkStmt (IfThen exp stmts) 
  = 
    do
      if not ((getExpType exp)==(BaseDataType BooleanType))then
          liftEither $ throwError ("IF exp is not boolean type")
      else
        do
          checkStmts stmts
  
checkStmt (IfThenElse exp stmts1 stmts2) 
  = 
    do
      if not ((getExpType exp)==(BaseDataType BooleanType))then
          liftEither $ throwError ("IF exp is not boolean type")
      else
        do
          checkStmts stmts1
          checkStmts stmts2

checkStmt (While exp stmts ) 
  = 
    do
      if not ((getExpType exp)==(BaseDataType BooleanType))then
          liftEither $ throwError ("IF exp is not boolean type")
      else
        do
          checkStmts stmts
                            
------------------------call-------------------------------- 

checkStmt (Call procedureName exps) 
  = 
    do
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
          if not (hasSameElem exps proParams) 
          then liftEither $ throwError ("parameters' types are not matched with what you call")
          else 
            do
              return ()
              -- TODO check type matche
          -- keeps checking the proedure being called
          --不应该call一次检查一次stmt，只用检查call的名字和参数就行
          --    pushLocalVariableTable
          --    insertProcedureVariable procCalled
          --    let (Procedure _ (ProcedureBody _ procCalledStmts)) = procCalled
          --    checkStmts procCalledStmts
          --    popLocalVariableTable
          
          

-- checkStmt _ = return ()

----TODO 补充完整
-- getExpType::Exp->SymTableState DataType

-- getExpType (BoolConst _)= do 
--   return (BaseDataType BooleanType)
-- getExpType (IntConst _)= do return (BaseDataType IntegerType)
-- getExpType (StrConst a)=do return (AliasDataType a)--TODO?
-- getExpType (Op_or _ _)=do return (BaseDataType BooleanType)
-- getExpType (Op_and _ _)=do return (BaseDataType BooleanType)
-- getExpType (Op_eq  _ _)=do return (BaseDataType BooleanType)
-- getExpType (Op_neq  _ _)=do return (BaseDataType BooleanType)
-- getExpType (Op_less  _ _)=do return (BaseDataType BooleanType)
-- getExpType (Op_less_eq  _ _)=do return (BaseDataType BooleanType)
-- getExpType (Op_large _ _)=do return (BaseDataType BooleanType)
-- getExpType (Op_large_eq _ _)=do return (BaseDataType BooleanType)
-- getExpType (Op_not _)=do return (BaseDataType BooleanType)
-- getExpType (Op_add _ _)=do return (BaseDataType IntegerType)
-- getExpType (Op_sub _ _)=do return (BaseDataType IntegerType)
-- getExpType (Op_mul _ _)=do return (BaseDataType IntegerType)
-- getExpType (Op_div _ _)=do return (BaseDataType IntegerType)
-- getExpType (Op_neg _)=do return (BaseDataType IntegerType)

-- getExpType (Lval lvalue )=getDatatypeoflvalue lvalue--TODO


-- data LValue 
--   = LId Ident                     -- <id>
--   | LDot Ident Ident              -- <id>.<id>
--   | LBrackets Ident Exp           -- <id>[<exp>]
--   | LBracketsDot Ident Exp Ident  -- <id>[<exp>].<id>
--     deriving (Show, Eq)

getExpType::Exp->DataType

getExpType (BoolConst _)=BaseDataType BooleanType
getExpType (IntConst _)=BaseDataType IntegerType
getExpType (StrConst a)=AliasDataType a--TODO
getExpType (Op_or _ _)=BaseDataType BooleanType
getExpType (Op_and _ _)=BaseDataType BooleanType
getExpType (Op_eq  _ _)=BaseDataType BooleanType
getExpType (Op_neq  _ _)=BaseDataType BooleanType
getExpType (Op_less  _ _)=BaseDataType BooleanType
getExpType (Op_less_eq  _ _)=BaseDataType BooleanType
getExpType (Op_large _ _)=BaseDataType BooleanType
getExpType (Op_large_eq _ _)=BaseDataType BooleanType
getExpType (Op_not _)=BaseDataType BooleanType
getExpType (Op_add _ _)=BaseDataType IntegerType
getExpType (Op_sub _ _)=BaseDataType IntegerType
getExpType (Op_mul _ _)=BaseDataType IntegerType
getExpType (Op_div _ _)=BaseDataType IntegerType
getExpType (Op_neg _)=BaseDataType IntegerType

getExpType (Lval _ )=BaseDataType BooleanType
---sDTtoDT::SymTableState DataType->DataType
--sDTtoDT y@(SymTableState x) =x

--call exp(parameter)==procedure table parameter   
hasSameElem::[Exp]->[(Bool, DataType)]->Bool
hasSameElem (x:xs) ((_,y):ys)=
  if (getExpType x) == y 
  then hasSameElem xs ys 
  else False
hasSameElem [] []=True
hasSameElem _ _=True

-----------semantic check on all kinds of expression----
checkExp::Exp->SymTableState ()
checkExp (BoolConst bbool)
  =return()


-----------semantic check on all operation------
--The language is statically typed, that is, each variable and parameter has a fixed type, chosen
--by the programmer. The type rules for expressions are as follows:
-- • The type of a Boolean constant is boolean.
-- • The type of an integer constant is integer.
-- • The type of a string literal is string.
-- • The type of an expression id is the variable id’s declared type. If the declaration uses a
-- type alias, the type is the one given by the type definition for that alias.
-- • For an expression lval .fname, lval must be of record type. The type of an expression
-- lval .fname is the type associated with field name fname, as given in the record type
-- associated with lval .
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
--all defined procedures must have distinct names.
--A defined procedure does not have to be called anywhere,(?) 
--the definition of a procedure does not have to precede the (textually) first call to the procedure.
--Procedures can use (mutual) recursion.
--the number of actual parameters in a call must be equal to the number of formal parameters in the procedure’s definition.


--The scope of a declared variable (or of a formal procedure parameter) is the enclosing procedure definition.
--A variable must be declared (exactly once) before it is used.
--a list of formal parameters must all be distinct
--the same variable/parameter name can be used in different procedures.

--In a record type definition, all field names must be given types boolean or integer.
--In an array type definition array [n] ..., n must be a positive integer.
--Each reference to an array variable must include exactly one index expression.

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

-- The language allows for two ways of passing parameters. Call by value is a copying mechanism. 
-- For each parameter e passed by value, the called procedure considers the corresponding formal 
-- parameter v a local variable and initialises this local variable to e’s value. Call by value 
-- is specified through the keyword val.
-- Call by reference does not involve copying.
-- Instead the called procedure is provided with the address of the actual parameter (which must be
-- a variable z, field reference x.a, or array element a[e] and the formal parameter v is considered
-- a synonym for the actual parameter.

-- 该语言允许两种方式传递参数。 按值调用是一种复制机制。 对于每个由值传递的参数e，被调用的过程都将对应的形
-- 式参数v视为局部变量，并将该局部变量初始化为e的值。 通过关键字val指定按值调用。
-- 通过引用调用不涉及复制。
-- 而是为被调用的过程提供了实际参数的地址（该地址必须是变量z，字段引用x.a或数组元素a [e]，并且形式参数v被视为实际参数的同义词。

