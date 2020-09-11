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

surroundByParens :: String -> String
surroundByParens s = "(" ++ s ++ ")"

surroundByBrackets :: String -> String
surroundByBrackets s = "[" ++ s ++ "]"

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
  procedureName ++ " " ++ (surroundByParens (intercalate ", " (map strParameter parameters)))



-- variableDecls can be empty
-- stmts is nonempty
strProcedureBody :: ProcedureBody -> String
-- Within each procedure, declarations and top-level statements should be indented.
strProcedureBody (ProcedureBody variableDecls stmts) = ""

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
strProgram (Program [] [] procedures) = concatMap strProcedure procedures
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
