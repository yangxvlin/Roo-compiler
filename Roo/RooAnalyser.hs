-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yang, Wenrui Zhang             --
--  Implemented by Team: GNU_project                     --
-----------------------------------------------------------
module RooAnalyser(analyse, Result(..)) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import RooAST
import SymbolTable
import qualified Data.Map as Map
import Data.Either

type Result = Either String SymTable

analyse :: Program -> Result 
analyse prog
  = evalStateT (semanticCheckRooProgram prog) initialSymTable

semanticCheckRooProgram :: Program -> SymTableState SymTable
semanticCheckRooProgram prog
  =
    do
      constructSymbolTable prog
      checkDistinctTypeAlias
      mainProcedure <- getProcedureDefinition "main"
      st <- get
      return st


constructSymbolTable :: Program -> SymTableState ()
constructSymbolTable prog@(Program records arraies procedures)
  = 
    do
      st <- get
      mapM_ insertRecordType records
      mapM_ insertArrayType arraies
      mapM_ insertProcedureDefinition procedures

-- all type aliases must be distinct, record and array has no overlapping name
checkDistinctTypeAlias :: SymTableState ()
checkDistinctTypeAlias
  =
    do
      st <- get
      -- has overlapping name in type alias
      if Map.size (Map.intersection (att st) (rtt st)) > 0 then
        liftEither $ throwError ("Duplicated type alias name")
      else
        return ()

-- Within a given procedure, variable names, including formal parameter names, 
-- must be distinct.

-- procedure's parameter and local variable should have different name



-----------------------------------------------------------
--Static semantics
-----------------------------------------------------------
--by value or by reference
-----------------------------------------------------------
--If the formal parameter’s type is followed by the keyword val → it has value mode.
--                                                      If not  → it has reference mode.

--Any expression of boolean or integer type can be passed   by value, as an argument to a procedure
--Some expressions of boolean or integer type can be passed by reference,namely those that are “lvalues”.
--                                                lvalues:  -a variable x (of primitive type)
--                                                          -an array element id[e] (of primitive type)
--                                                          -a field reference x.a 
--                                                          -id[e].a.

--A formal parameter with value mode behaves exactly as if it were a local variable.So：
--A “by reference” formal parameter                       has reference mode, 
--while other formal parameters, and all local variables, have value mode.

--A record or an array may be passed as a whole, but in that case it must be passed by reference.
--Similarly, an assignment lval <- e, where e has record or array type, is only allowed if lval and e both have reference mode.

-- 该语言允许两种方式传递参数。 按值调用是一种复制机制。 对于每个由值传递的参数e，被调用的过程都将对应的形
-- 式参数v视为局部变量，并将该局部变量初始化为e的值。 通过关键字val指定按值调用。
-- 通过引用调用不涉及复制。
-- 而是为被调用的过程提供了实际参数的地址（该地址必须是变量z，字段引用x.a或数组元素a [e]，并且形式参数v被视为实际参数的同义词。

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
-- • The operand of unary minus must be of type integer, and the result type is the same.

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
-- Every procedure in a program must be well-typed, that is, its body must be a sequence of
-- well-typed statements. Every program must contain a procedure of arity 0 named “main”.

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

--write and read:
--  write prints integer and boolean expressions to stdout in their standard syntactic forms, 
--with no additional whitespace or newlines.
--  If write is given a string, it prints out the characters of the string to stdout, with \" 
--resulting in a double quote being printed, \n in a newline character being printed, and \t 
--in a tab being printed.
--  writeln behaves exactly like write, except it prints an additional, final newline character.
--  Similarly, read reads an integer or boolean literal from stdin and assigns it to an lvalue.
--If the user input is not valid, execution terminates.

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
