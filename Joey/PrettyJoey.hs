module PrettyJoey (pp)
where
import JoeyAST
import Data.List

--  Pretty print a whole program:

pp :: Program -> String
pp = strProgram

-----------------------------------------------------------------
--  helper functions
-----------------------------------------------------------------

-- add space indentations based on the input indentation level
addIndentation :: Int -> String
addIndentation 0 = ""
addIndentation n = "    " ++ addIndentation (n - 1)

newline :: String
newline = "\n"

semicolon :: String
semicolon = ";"

comma :: String
-- There should be a single space after a comma
comma = ", "

surroundByParens :: String -> String
surroundByParens s = "(" ++ s ++ ")"

surroundByBrackets :: String -> String
surroundByBrackets s = "[" ++ s ++ "]"

-- replace \n, \t to \\n, \\t so that they can be printed as string in the output not escape
strEscape :: String -> String
strEscape ('\n':xs) =  "\\n" ++ strEscape xs
strEscape ('\t':xs) =  "\\t" ++ strEscape xs
strEscape (x:xs)    = x : strEscape xs
strEscape ""        = ""

-----------------------------------------------------------------
--  AST toString functions 
--  read from bottom to top
-----------------------------------------------------------------

strBaseType :: BaseType -> String
strBaseType BooleanType = "boolean"
strBaseType IntegerType = "integer"

strDataType :: DataType -> String
strDataType (AliasDataType t) = t
strDataType (BasyDataType b) = strBaseType b

-----------------------------------------------------------------
--  
-----------------------------------------------------------------
strLValue :: LValue -> String
strLValue (LId ident) = ident
strLValue (LDot ident1 ident2) = ident1 ++ "." ++ ident2
strLValue (LBrackets ident exp) = ident ++ (surroundByBrackets (strExp exp))
strLValue (LBracketsDot ident1 exp ident2) = ident1 ++ (surroundByBrackets (strExp exp)) ++ "." ++ ident2

strExp :: Exp -> String
-- <lvalue>
strExp (Lval lValue) = strLValue lValue
-- <const>
strExp (BoolConst booleanLiteral) = show booleanLiteral
-- <const>
strExp (IntConst integerLiteral) = show integerLiteral
-- <const>
-- White space, and upper/lower case, should be preserved inside strings.
strExp (StrConst stringLiteral) = "\"" ++ (strEscape stringLiteral) ++ "\""
-- <exp> <binop> <exp>
strExp _ = ""


-- Int: indentation level
strStmt :: Int -> Stmt -> String
-- In a procedure body, each statement should start on a new line. So ++ newline in each end
strStmt indentLevel (Assign lValue exp) = 
  -- <lvalue> <- <exp> ;
  -- Single spaces should surround the assignment operator <-
  (addIndentation indentLevel) ++ (strLValue lValue) ++ " <- " ++ (strExp exp) ++ semicolon ++ newline
strStmt indentLevel (Read lValue) = 
  -- read <lvalue>;
  (addIndentation indentLevel) ++ "read " ++ (strLValue lValue) ++ semicolon ++ newline
strStmt indentLevel (Write exp) =
  -- write <exp>;
  (addIndentation indentLevel) ++ "write " ++ (strExp exp) ++ semicolon ++ newline
strStmt indentLevel (Writeln exp) = 
  -- writeln <exp>;
  (addIndentation indentLevel) ++ "writeln " ++ (strExp exp) ++ semicolon ++ newline
strStmt indentLevel (Call ident exps) =
  -- call <id> ( <exp-list> ); 
  --     where <exp-list> is a (possibly empty) comma-separated list of expressions.
  (addIndentation indentLevel) ++ "call " ++ ident ++ " " ++ surroundByParens (intercalate comma (map strExp exps)) ++ semicolon ++ newline
-- thenStmts is non-empty, elseStmts is possible empty
strStmt indentLevel (If exp thenStmts elseStmts) = 
  -- terminating fi (and a possible else) should be indented exactly as the corresponding if.
  -- IF elseStmts empty: if <exp> then <stmt-list> fi
  if null elseStmts then
    -- In a while statement, "if ... then" should be printed on one line, 
    -- irrespective of the size of the intervening expression
    (addIndentation indentLevel) ++ "if " ++ (strExp exp) ++ " then" ++ newline ++
    (concatMap (strStmt (indentLevel+1)) thenStmts) ++
    -- the terminating fi should be indented exactly as the corresponding if.
    (addIndentation indentLevel) ++ "fi" ++ newline
  -- OTHERWISE:          if <expr> then <stmt-list> else <stmt-list> fi
  else
    -- In a while statement, "if ... then" should be printed on one line, 
    -- irrespective of the size of the intervening expression
    (addIndentation indentLevel) ++ "if " ++ (strExp exp) ++ " then" ++ newline ++
    (concatMap (strStmt (indentLevel+1)) thenStmts) ++
    (addIndentation indentLevel) ++ "else" ++ newline ++
    (concatMap (strStmt (indentLevel+1)) elseStmts) ++
    -- the terminating fi and else should be indented exactly as the corresponding if.
    (addIndentation indentLevel) ++ "fi" ++ newline
-- stmts is non-empty
strStmt indentLevel (While exp stmts) = 
  -- In a while statement, "while ... do" should be printed on one line, 
  -- irrespective of the size of the intervening expression
  (addIndentation indentLevel) ++ "while " ++ (strExp exp) ++ " do" ++ newline ++
  (concatMap (strStmt (indentLevel+1)) stmts) ++
  -- The terminating od should be indented exactly as the corresponding while.
  (addIndentation indentLevel) ++ "od" ++ newline

-----------------------------------------------------------------
--  
-----------------------------------------------------------------
strFieldDecl :: FieldDecl -> String
strFieldDecl (FieldDecl baseType fieldName) = (show baseType) ++ fieldName

-- non empty input list of FieldDecl
strFieldDecls :: [FieldDecl] -> String
-- first field decl starts with {
-- rest starts with ;
strFieldDecls (x:xs) = (addIndentation 1) ++ "{ " ++ (strFieldDecl x) ++ newline ++
                       (concatMap 
                          (\y -> (addIndentation 1) ++ "; " ++ (strFieldDecl y) ++ newline) 
                          xs
                       )

-- convert record to string
-- A record type definition involving n fields should be printed on n + 2 lines, 
-- as follows:
--     The first line contains the word record. The remaining lines should be 
--     indented, with the first n containing one field declaration each (the 
--     first preceded by a left brace and a single space, the rest preceded by 
--     a semicolon and a single space), and with the last line containing the 
--     record name, preceded by a right brace and a single space, and followed 
--     by a semicolon;
strRecord :: Record -> String
strRecord (Record fieldDecls recordName) = 
  "record" ++ newline ++
  (strFieldDecls fieldDecls) ++ 
  (addIndentation 1) ++ "} " ++ recordName ++ semicolon ++ newline

-----------------------------------------------------------------
--  
-----------------------------------------------------------------
-- convert array to string
-- An array type definition should be printed on a single line. 
-- It contains the word array, 
-- followed by a positive integer in square brackets all without intervening 
--      white space.
-- That string, the type, and the type alias, should be separated by single 
-- spaces, and the whole line terminated by a semicolon.
strArray :: Array -> String
strArray (Array arraySize arrayType arrayName) = 
  "array" ++ surroundByBrackets (show arraySize) ++ " " ++ (strDataType arrayType) ++ " " ++ arrayName ++ semicolon ++ newline

-----------------------------------------------------------------
--  
-----------------------------------------------------------------
strParameter :: Parameter -> String
strParameter (DataParameter dataType paraName) = (strDataType dataType) ++ " " ++ paraName
strParameter (BooleanParameter booleanLiteral) = show booleanLiteral
strParameter (IntegerParameter integerLiteral) = show integerLiteral

strProcedureHeader :: ProcedureHeader -> String
strProcedureHeader (ProcedureHeader procedureName parameters) = 
  procedureName ++ " " ++ (surroundByParens (intercalate comma (map strParameter parameters)))

strVariableDecl :: VariableDecl -> String
strVariableDecl (VariableDecl dataType varNames) = 
  -- Within each procedure, declarations and top-level statements should be indented.
  (addIndentation 1) ++ (strDataType dataType) ++ " " ++ (intercalate comma varNames)  ++ semicolon ++
  -- Each variable declaration should be on a separate line.
  newline

-- variableDecls can be empty
-- stmts is nonempty
strProcedureBody :: ProcedureBody -> String
-- Within each procedure, declarations and top-level statements should be indented.
strProcedureBody (ProcedureBody variableDecls stmts) = 
  (concatMap strVariableDecl variableDecls) ++
  -- The { and } that surround a procedure body should begin at the start of a 
  -- line: no indentation. Moreover, these delimiters should appear alone, each 
  -- making up a single line.
  "{" ++ newline ++ 
  concatMap (strStmt 1) stmts ++ 
  "}" ++ newline

-- convert procedure to string
strProcedure :: Procedure -> String
-- The keyword procedure should begin at the start of a line--no indentation. 
-- The procedure head (that is, the keyword, procedure name, and list of formal 
--     parameters) should be on a single line.
strProcedure (Procedure ph pb) =
  "procedure " ++ (strProcedureHeader ph) ++ newline ++
  (strProcedureBody pb)


-----------------------------------------------------------------
--  
-----------------------------------------------------------------
-- convert program to string
strProgram :: Program -> String
-- If there are no record and array type definitions, the first procedure should 
-- start on line 1.
strProgram (Program [] [] procedures) = intercalate newline (map strProcedure procedures)
-- Otherwise there should be a single blank line between the type definitions 
-- and the first procedure.
strProgram (Program records arraies procedures) = 
  -- Each type definition should start on a new line, and there should be no 
  -- blank lines between type definitions. So below two has no newline in between
  concatMap strRecord records ++
  concatMap strArray arraies ++
  newline ++ 
  -- Consecutive procedure definitions should be separated by a single blank line.
  intercalate newline (map strProcedure procedures)
