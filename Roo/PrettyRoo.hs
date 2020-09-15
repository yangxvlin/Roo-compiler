module PrettyRoo (pp)
where
import RooAST
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
-- strEscape :: String -> String
-- strEscape ('\n':xs) =  "\\n" ++ strEscape xs
-- strEscape ('\t':xs) =  "\\t" ++ strEscape xs
-- strEscape (x:xs)    = x : strEscape xs
-- strEscape ""        = ""

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

strBooleanLiteral :: BooleanLiteral -> String
strBooleanLiteral True = "true"
strBooleanLiteral False = "false"

-----------------------------------------------------------------
--  
-----------------------------------------------------------------
strLValue :: LValue -> String
strLValue (LId ident) = ident
strLValue (LDot ident1 ident2) = ident1 ++ "." ++ ident2
strLValue (LBrackets ident exp) = ident ++ (surroundByBrackets (strExp exp))
strLValue (LBracketsDot ident1 exp ident2) = ident1 ++ (surroundByBrackets (strExp exp)) ++ "." ++ ident2

-- exp1 has higher precendence than exp2, higher if exp2 has no operator
isHigherPrecendence :: Exp -> Exp -> Bool
isHigherPrecendence exp1@(Op_neg _) exp2 = 
  case exp2 of
    (Op_neg _  ) -> False
    _            -> True
isHigherPrecendence exp1@(Op_mul _ _) exp2 = 
  case exp2 of
    (Op_neg _  ) -> False
    (Op_mul _ _) -> False
    (Op_div _ _) -> False
    _            -> True
isHigherPrecendence exp1@(Op_div _ _) exp2 = 
  case exp2 of
    (Op_neg _  ) -> False
    (Op_mul _ _) -> False
    (Op_div _ _) -> False
    _            -> True
isHigherPrecendence exp1@(Op_add _ _) exp2 = 
  case exp2 of
    (Op_neg _  ) -> False
    (Op_mul _ _) -> False
    (Op_div _ _) -> False
    (Op_add _ _) -> False
    (Op_sub _ _) -> False
    _            -> True
isHigherPrecendence exp1@(Op_sub _ _) exp2 = 
  case exp2 of
    (Op_neg _  ) -> False
    (Op_mul _ _) -> False
    (Op_div _ _) -> False
    (Op_add _ _) -> False
    (Op_sub _ _) -> False
    _            -> True
isHigherPrecendence exp1@(Op_eq _ _) exp2 = 
  case exp2 of
    (Op_not _  )  -> True
    (Op_and _ _)  -> True
    (Op_or  _ _)  -> True
    -- constants has lowest precendence
    (Lval _) -> True
    (BoolConst _) -> True
    (IntConst _) -> True
    (StrConst _) -> True
    _             -> False
isHigherPrecendence exp1@(Op_neq _ _) exp2 = 
  case exp2 of
    (Op_not _  )  -> True
    (Op_and _ _)  -> True
    (Op_or  _ _)  -> True
    -- constants has lowest precendence
    (Lval _) -> True
    (BoolConst _) -> True
    (IntConst _) -> True
    (StrConst _) -> True
    _             -> False
isHigherPrecendence exp1@(Op_less _ _) exp2 = 
  case exp2 of
    (Op_not _  )  -> True
    (Op_and _ _)  -> True
    (Op_or  _ _)  -> True
    -- constants has lowest precendence
    (Lval _) -> True
    (BoolConst _) -> True
    (IntConst _) -> True
    (StrConst _) -> True
    _             -> False
isHigherPrecendence exp1@(Op_less_eq _ _) exp2 = 
  case exp2 of
    (Op_not _  )  -> True
    (Op_and _ _)  -> True
    (Op_or  _ _)  -> True
    -- constants has lowest precendence
    (Lval _) -> True
    (BoolConst _) -> True
    (IntConst _) -> True
    (StrConst _) -> True
    _             -> False
isHigherPrecendence exp1@(Op_large _ _) exp2 = 
  case exp2 of
    (Op_not _  )  -> True
    (Op_and _ _)  -> True
    (Op_or  _ _)  -> True
    -- constants has lowest precendence
    (Lval _) -> True
    (BoolConst _) -> True
    (IntConst _) -> True
    (StrConst _) -> True
    _             -> False
isHigherPrecendence exp1@(Op_large_eq _ _) exp2 = 
  case exp2 of
    (Op_not _  )  -> True
    (Op_and _ _)  -> True
    (Op_or  _ _)  -> True
    -- constants has lowest precendence
    (Lval _) -> True
    (BoolConst _) -> True
    (IntConst _) -> True
    (StrConst _) -> True
    _             -> False
isHigherPrecendence exp1@(Op_not _) exp2 = 
  case exp2 of
    (Op_and _ _)  -> True
    (Op_or  _ _)  -> True
    -- constants has lowest precendence
    (Lval _) -> True
    (BoolConst _) -> True
    (IntConst _) -> True
    (StrConst _) -> True
    _             -> False
isHigherPrecendence exp1@(Op_and _ _) exp2 = 
  case exp2 of
    (Op_or  _ _)  -> True
    -- constants has lowest precendence
    (Lval _) -> True
    (BoolConst _) -> True
    (IntConst _) -> True
    (StrConst _) -> True
    _             -> False
isHigherPrecendence exp1@(Op_or _ _) exp2
   -- constants has lowest precendence
  | (not (isOperatorExp exp2)) = True
  | otherwise = False
isHigherPrecendence _ _ = False

-- exp1 has higher precendence than exp2
isSamePrecendence :: Exp -> Exp -> Bool
isSamePrecendence exp1@(Op_neg _) exp2 = 
  case exp2 of
    (Op_neg _  ) -> True
    _            -> False
isSamePrecendence exp1@(Op_mul _ _) exp2 = 
  case exp2 of
    (Op_mul _ _) -> True
    (Op_div _ _) -> True
    _            -> False
isSamePrecendence exp1@(Op_div _ _) exp2 = 
  case exp2 of
    (Op_mul _ _) -> True
    (Op_div _ _) -> True
    _            -> False
isSamePrecendence exp1@(Op_add _ _) exp2 = 
  case exp2 of
    (Op_add _ _) -> True
    (Op_sub _ _) -> True
    _            -> False
isSamePrecendence exp1@(Op_sub _ _) exp2 = 
  case exp2 of
    (Op_add _ _) -> True
    (Op_sub _ _) -> True
    _            -> False
isSamePrecendence exp1@(Op_eq _ _) exp2 = 
  case exp2 of
    -- (Op_eq _ _) -> True
    -- (Op_neq _ _) -> True
    -- (Op_less _ _) -> True
    -- (Op_less_eq _ _) -> True
    -- (Op_large _ _) -> True
    -- (Op_large_eq _ _) -> True
    _             -> False -- relational
isSamePrecendence exp1@(Op_neq _ _) exp2 = 
  case exp2 of
    -- (Op_eq _ _) -> True
    -- (Op_neq _ _) -> True
    -- (Op_less _ _) -> True
    -- (Op_less_eq _ _) -> True
    -- (Op_large _ _) -> True
    -- (Op_large_eq _ _) -> True
    _             -> False -- relational
isSamePrecendence exp1@(Op_less _ _) exp2 = 
  case exp2 of
    -- (Op_eq _ _) -> True
    -- (Op_neq _ _) -> True
    -- (Op_less _ _) -> True
    -- (Op_less_eq _ _) -> True
    -- (Op_large _ _) -> True
    -- (Op_large_eq _ _) -> True
    _             -> False -- relational
isSamePrecendence exp1@(Op_less_eq _ _) exp2 = 
  case exp2 of
    -- (Op_eq _ _) -> True
    -- (Op_neq _ _) -> True
    -- (Op_less _ _) -> True
    -- (Op_less_eq _ _) -> True
    -- (Op_large _ _) -> True
    -- (Op_large_eq _ _) -> True
    _             -> False -- relational
isSamePrecendence exp1@(Op_large _ _) exp2 = 
  case exp2 of
    -- (Op_eq _ _) -> True
    -- (Op_neq _ _) -> True
    -- (Op_less _ _) -> True
    -- (Op_less_eq _ _) -> True
    -- (Op_large _ _) -> True
    -- (Op_large_eq _ _) -> True
    _             -> False
isSamePrecendence exp1@(Op_large_eq _ _) exp2 = 
  case exp2 of
    -- (Op_eq _ _) -> True
    -- (Op_neq _ _) -> True
    -- (Op_less _ _) -> True
    -- (Op_less_eq _ _) -> True
    -- (Op_large _ _) -> True
    -- (Op_large_eq _ _) -> True
    _             -> False -- relational
isSamePrecendence exp1@(Op_not _) exp2 = 
  case exp2 of
    (Op_not _)  -> True
    _             -> False
isSamePrecendence exp1@(Op_and _ _) exp2 = 
  case exp2 of
    (Op_and _ _)  -> True
    _             -> False
isSamePrecendence exp1@(Op_or _ _) exp2 =
  case exp2 of
    (Op_or  _ _)  -> True
    _             -> False
isSamePrecendence _ _ = False

-- exp1 has smaller precendence than exp2 if it is not higher nor same
isSamllerPrecendence :: Exp -> Exp -> Bool
isSamllerPrecendence exp1 exp2 = (not (isHigherPrecendence exp1 exp2)) &&
                                 (not (isSamePrecendence   exp1 exp2))

-- two expresions have same operator
isSameOperation :: Exp -> Exp -> Bool
isSameOperation (Op_or _ _) (Op_or _ _) = True
isSameOperation (Op_and _ _) (Op_and _ _) = True
isSameOperation (Op_eq _ _) (Op_eq _ _) = True
isSameOperation (Op_neq _ _) (Op_neq _ _) = True
isSameOperation (Op_less _ _) (Op_less _ _) = True
isSameOperation (Op_less_eq _ _) (Op_less_eq _ _) = True
isSameOperation (Op_large _ _) (Op_large _ _) = True
isSameOperation (Op_large_eq _ _) (Op_large_eq _ _) = True
isSameOperation (Op_add _ _) (Op_add _ _) = True
isSameOperation (Op_sub _ _) (Op_sub _ _) = True
isSameOperation (Op_mul _ _) (Op_mul _ _) = True
isSameOperation (Op_div _ _) (Op_div _ _) = True
isSameOperation (Op_not _) (Op_not _) = True
isSameOperation (Op_neg _) (Op_neg _) = True
isSameOperation _ _ = False

-- does expression has operator
isOperatorExp :: Exp -> Bool
isOperatorExp (Lval _) = False
isOperatorExp (BoolConst _) = False
isOperatorExp (IntConst _) = False
isOperatorExp (StrConst _) = False
isOperatorExp _ = True

-- return true if parent is sub/div and child has same precendence as parent
--    pexp: parent expression
--    cexp: child  expression
isDivSubParentSamePrecChild :: Exp -> Exp -> Bool
isDivSubParentSamePrecChild pexp@(Op_div _ _) cexp@(Op_div _ _) = True  -- / with a right child of / need a parens
isDivSubParentSamePrecChild pexp@(Op_div _ _) cexp@(Op_mul _ _) = True  -- / with a right child of * need a parens
isDivSubParentSamePrecChild pexp@(Op_sub _ _) cexp@(Op_sub _ _) = True  -- - (sub) with a right child of - (sub) need a parens
isDivSubParentSamePrecChild pexp@(Op_sub _ _) cexp@(Op_add _ _) = True  -- - (sub) with a right child of + need a parens
isDivSubParentSamePrecChild _ _ = False

-- 3 * (5 / 3) = 3 * 1 = 3, but (3 * 5) / 3 = 15 / 3 = 5,  so parens needed
isIntegerDision :: Exp -> Exp -> Bool
isIntegerDision pexp@(Op_mul _ _) cexp@(Op_div _ _) = True 
isIntegerDision _ _ = False

-- return true a not with constant after a relational expression doesn't need parens
--    pexp: parent       expression
--    exp2: right child  expression
-- isRelationalRNotThenNoParens :: Exp -> Exp -> Bool

-- some notation:
--    pexp: parent      expression (definitely has operator)
--    exp1: left child  expression
--    exp2: right child expression
-- turn binary expression's left child to string
strBinaryExpLChild :: Exp -> Exp -> String
strBinaryExpLChild pexp exp1
  | (isSamllerPrecendence exp1 pexp) && (isOperatorExp exp1) = surroundByParens (strExp exp1) -- left child (with operator) has lower precendence suggests a parens
  | otherwise = strExp exp1 -- no parens

-- some notation:
--    pexp: parent      expression (definitely has operator)
--    exp1: left child  expression
--    exp2: right child expression
-- turn binary expression's right child to string
strBinaryExpRChild :: Exp -> Exp -> String
strBinaryExpRChild pexp exp2
  | (isOperatorExp exp2) && ((isDivSubParentSamePrecChild pexp exp2) || 
                             (isSamllerPrecendence exp2 pexp) ||
                             (isIntegerDision pexp exp2)
                            ) 
    = surroundByParens (strExp exp2) -- right child (with operator) has lower precendence suggests a parens
  | otherwise = strExp exp2 -- no parens

-- some notation:
--    pexp: parent      expression
--    exp1: left child  expression
--    exp2: right child expression
strExp :: Exp -> String
-- <lvalue>
strExp (Lval lValue) = strLValue lValue
-- <const>
strExp (BoolConst booleanLiteral) = strBooleanLiteral booleanLiteral
-- <const>
strExp (IntConst integerLiteral) = show integerLiteral
-- <const>
-- White space, and upper/lower case, should be preserved inside strings.
-- strExp (StrConst stringLiteral) = "\"" ++ (strEscape stringLiteral) ++ "\""
strExp (StrConst stringLiteral) = show stringLiteral

-- "-(binary/unary expressions)"
-- no space after unary minus
strExp pexp@(Op_neg exp)
  | (isOperatorExp exp) && (not (isSamePrecendence pexp exp)) = "-" ++ surroundByParens (strExp exp) -- need to parens expression with operator; --5 no parens
  | otherwise         = "-" ++ (strExp exp) -- no need to parens constants

-- <exp> <binop> <exp>
-- Single spaces should surround 12 binary operators.
-- * binary expression
strExp pexp@(Op_mul exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " * " ++ (strBinaryExpRChild pexp exp2)

-- <exp> <binop> <exp>
-- Single spaces should surround 12 binary operators.
-- / binary expression
strExp pexp@(Op_div exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " / " ++ (strBinaryExpRChild pexp exp2)

-- <exp> <binop> <exp>
-- Single spaces should surround 12 binary operators.
-- + binary expression
strExp pexp@(Op_add exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " + " ++ (strBinaryExpRChild pexp exp2)

-- <exp> <binop> <exp>
-- Single spaces should surround 12 binary operators.
-- - binary expression
strExp pexp@(Op_sub exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " - " ++ (strBinaryExpRChild pexp exp2)

-- <exp> <binop> <exp>
-- Single spaces should surround 12 binary operators.
-- = binary expression
strExp pexp@(Op_eq exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " = " ++ (strBinaryExpRChild pexp exp2)

-- <exp> <binop> <exp>
-- Single spaces should surround 12 binary operators.
-- != binary expression
strExp pexp@(Op_neq exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " != " ++ (strBinaryExpRChild pexp exp2)

-- <exp> <binop> <exp>
-- Single spaces should surround 12 binary operators.
-- < binary expression
strExp pexp@(Op_less exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " < " ++ (strBinaryExpRChild pexp exp2)

-- <exp> <binop> <exp>
-- Single spaces should surround 12 binary operators.
-- <= binary expression
strExp pexp@(Op_less_eq exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " <= " ++ (strBinaryExpRChild pexp exp2)

-- <exp> <binop> <exp>
-- Single spaces should surround 12 binary operators.
-- > binary expression
strExp pexp@(Op_large exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " > " ++ (strBinaryExpRChild pexp exp2)

-- <exp> <binop> <exp>
-- Single spaces should surround 12 binary operators.
-- >= binary expression
strExp pexp@(Op_large_eq exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " >= " ++ (strBinaryExpRChild pexp exp2)

-- <exp> <binop> <exp>
-- There should be a single space after not.
-- not unary expression
strExp pexp@(Op_not exp) 
  -- need to parens expression with operator and (child's precendence is same or smaller)
  | (isOperatorExp exp) && (isSamllerPrecendence exp pexp) = "not " ++ surroundByParens (strExp exp)
  | otherwise         = "not " ++ (strExp exp) -- no need to parens constants

-- <exp> <binop> <exp>
-- Single spaces should surround 12 binary operators.
-- and binary expression
strExp pexp@(Op_and exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " and " ++ (strBinaryExpRChild pexp exp2)

-- <exp> <binop> <exp>
-- Single spaces should surround 12 binary operators.
-- or binary expression
strExp pexp@(Op_or exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " or " ++ (strBinaryExpRChild pexp exp2)


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
  -- call <id>(<exp-list>); 
  --     where <exp-list> is a (possibly empty) comma-separated list of expressions.
  (addIndentation indentLevel) ++ "call " ++ ident ++ surroundByParens (intercalate comma (map strExp exps)) ++ semicolon ++ newline
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
strFieldDecl (FieldDecl baseType fieldName) = (strBaseType baseType) ++ " " ++ fieldName

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
strParameter (BooleanVal paraName) = "boolean val " ++ paraName
strParameter (IntegerVal paraName) = "integer val " ++ paraName
-- strParameter (BooleanParameter booleanLiteral) = strBooleanLiteral booleanLiteral
-- strParameter (IntegerParameter integerLiteral) = show integerLiteral

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
