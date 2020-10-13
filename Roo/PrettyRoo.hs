-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yang                            --
--  read from the bottom to top                          --
-----------------------------------------------------------
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
comma = ", "  -- There should be a single space after a comma

surroundByParens :: String -> String
surroundByParens s = "(" ++ s ++ ")"

surroundByBrackets :: String -> String
surroundByBrackets s = "[" ++ s ++ "]"

-----------------------------------------------------------------
--  AST toString functions 
-----------------------------------------------------------------

strBaseType :: BaseType -> String
strBaseType BooleanType = "boolean"
strBaseType IntegerType = "integer"

strDataType :: DataType -> String
strDataType (AliasDataType t) = t -- t is String already
strDataType (BasyDataType b) = strBaseType b

strBool :: Bool -> String
strBool True = "true"
strBool False = "false"

-- String can be directly used as it is String type

-- Int can be turned to string by show

-----------------------------------------------------------------
-- An lvalue (<lvalue>) has four (and only four) possible forms:
--     <id>
--     <id>.<id>
--     <id>[<exp>]
--     <id>[<exp>].<id>
-----------------------------------------------------------------
strLValue :: LValue -> String
strLValue (LId ident) = ident
strLValue (LDot ident1 ident2) = ident1 ++ "." ++ ident2
strLValue (LBrackets ident exp) = ident ++ (surroundByBrackets (strExp exp))
strLValue (LBracketsDot ident1 exp ident2) = ident1 ++ (surroundByBrackets (strExp exp)) ++ "." ++ ident2

-----------------------------------------------------------------
-- Exp related toString & helper functions (with parens elimination)
-----------------------------------------------------------------
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
  | (not (hasOperatorExp exp2)) = True
  | otherwise = False
isHigherPrecendence _ _ = False

-- exp1 has same precendence as exp2
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
isSamePrecendence exp1@(Op_eq _ _) exp2       = False -- relational
isSamePrecendence exp1@(Op_neq _ _) exp2      = False -- relational
isSamePrecendence exp1@(Op_less _ _) exp2     = False -- relational
isSamePrecendence exp1@(Op_less_eq _ _) exp2  = False -- relational
isSamePrecendence exp1@(Op_large _ _) exp2    = False -- relational
isSamePrecendence exp1@(Op_large_eq _ _) exp2 = False -- relational
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

-- does expression has operator in it?
hasOperatorExp :: Exp -> Bool
hasOperatorExp (Lval _) = False
hasOperatorExp (BoolConst _) = False
hasOperatorExp (IntConst _) = False
hasOperatorExp (StrConst _) = False
hasOperatorExp _ = True

-- return true if parent is sub/div and child has same precendence as parent
--    pexp: parent expression
--    cexp: child  expression
isDivSubParentSamePrecChild :: Exp -> Exp -> Bool
isDivSubParentSamePrecChild pexp@(Op_div _ _) cexp@(Op_div _ _) = True  -- / with a right child of / need a parens
isDivSubParentSamePrecChild pexp@(Op_div _ _) cexp@(Op_mul _ _) = True  -- / with a right child of * need a parens
isDivSubParentSamePrecChild pexp@(Op_sub _ _) cexp@(Op_sub _ _) = True  -- - (sub) with a right child of - (sub) need a parens
isDivSubParentSamePrecChild pexp@(Op_sub _ _) cexp@(Op_add _ _) = True  -- - (sub) with a right child of + need a parens
isDivSubParentSamePrecChild _ _ = False

-- True if Integer division happens
--    Integer division: 3 * (5 / 3) = 3 * 1 = 3, but (3 * 5) / 3 = 15 / 3 = 5,  so parens needed
isIntegerDision :: Exp -> Exp -> Bool
isIntegerDision pexp@(Op_mul _ _) cexp@(Op_div _ _) = True 
isIntegerDision _ _ = False

-- some notation:
--    pexp: parent      expression (definitely has operator)
--    exp1: left child  expression
--    exp2: right child expression
-- turn binary expression's left child to string
strBinaryExpLChild :: Exp -> Exp -> String
strBinaryExpLChild pexp exp1
  -- left child (with operator) has lower precendence suggests a parens
  | (isSamllerPrecendence exp1 pexp) && (hasOperatorExp exp1) = surroundByParens (strExp exp1)
  -- no parens
  | otherwise = strExp exp1 

-- True if expression is "not" <exp>
isNotExp :: Exp -> Bool
isNotExp (Op_not _) = True
isNotExp _ = False

-- some notation:
--    pexp: parent      expression (definitely has operator)
--    exp1: left child  expression
--    exp2: right child expression
-- turn binary expression's right child to string
strBinaryExpRChild :: Exp -> Exp -> String
strBinaryExpRChild pexp exp2
  -- right child (with operator) has lower precendence (except "not" operator) or 
  --    same predence as parent (if parent is div(/) or sub(-)) or
  --    integer division happens
  -- suggests a parens
  | (hasOperatorExp exp2) && 
    ((isDivSubParentSamePrecChild pexp exp2) || 
      (isSamllerPrecendence exp2 pexp) ||
      (isIntegerDision pexp exp2)
    ) && 
    (not (isNotExp exp2)) 
      = surroundByParens (strExp exp2)
  -- no parens
  | otherwise = strExp exp2 

-- some notation:
--    pexp: parent      expression
--    exp1: left child  expression
--    exp2: right child expression
strExp :: Exp -> String
-- <lvalue>
strExp (Lval lValue) = strLValue lValue
-- <const>
strExp (BoolConst booleanLiteral) = strBool booleanLiteral
-- <const>
strExp (IntConst integerLiteral) = show integerLiteral
-- <const>
-- White space, and upper/lower case, should be preserved inside strings.
-- stringLiterals
strExp (StrConst stringLiteral) = "\"" ++ stringLiteral ++ "\""
-- <unop: "-"> <exp>
-- no space after unary minus
strExp pexp@(Op_neg exp)
  | (hasOperatorExp exp) && (not (isSamePrecendence pexp exp)) = "-" ++ surroundByParens (strExp exp) -- need to parens expression with operator; --5 no parens
  | otherwise         = "-" ++ (strExp exp) -- no need to parens constants/Lval
-- <exp> <binop: "*"> <exp>
-- Single space should surround 12 binary operators.
strExp pexp@(Op_mul exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " * " ++ (strBinaryExpRChild pexp exp2)
-- <exp> <binop: "/"> <exp>
-- Single space should surround 12 binary operators.
strExp pexp@(Op_div exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " / " ++ (strBinaryExpRChild pexp exp2)
-- <exp> <binop: "+"> <exp>
-- Single space should surround 12 binary operators.
strExp pexp@(Op_add exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " + " ++ (strBinaryExpRChild pexp exp2)
-- <exp> <binop: "-"> <exp>
-- Single space should surround 12 binary operators.
strExp pexp@(Op_sub exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " - " ++ (strBinaryExpRChild pexp exp2)
-- <exp> <binop: "="> <exp>
-- Single space should surround 12 binary operators.
strExp pexp@(Op_eq exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " = " ++ (strBinaryExpRChild pexp exp2)
-- <exp> <binop: "!="> <exp>
-- Single space should surround 12 binary operators.
strExp pexp@(Op_neq exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " != " ++ (strBinaryExpRChild pexp exp2)
-- <exp> <binop: "<"> <exp>
-- Single space should surround 12 binary operators.
strExp pexp@(Op_less exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " < " ++ (strBinaryExpRChild pexp exp2)
-- <exp> <binop: "<="> <exp>
-- Single space should surround 12 binary operators.
strExp pexp@(Op_less_eq exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " <= " ++ (strBinaryExpRChild pexp exp2)
-- <exp> <binop: ">"> <exp>
-- Single space should surround 12 binary operators.
strExp pexp@(Op_large exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " > " ++ (strBinaryExpRChild pexp exp2)
-- <exp> <binop: ">="> <exp>
-- Single space should surround 12 binary operators.
strExp pexp@(Op_large_eq exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " >= " ++ (strBinaryExpRChild pexp exp2)
-- <unop: not> <exp>
-- There should be a single space after not.
strExp pexp@(Op_not exp) 
  -- need to parens expression with operator and (child's precendence is smaller)
  | (hasOperatorExp exp) && (isSamllerPrecendence exp pexp) = "not " ++ surroundByParens (strExp exp)
  | otherwise         = "not " ++ (strExp exp) -- no need to parens constants/Lval
-- <exp> <binop: and> <exp>
-- Single space should surround 12 binary operators.
strExp pexp@(Op_and exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " and " ++ (strBinaryExpRChild pexp exp2)
-- <exp> <binop: or> <exp>
-- Single space should surround 12 binary operators.
strExp pexp@(Op_or exp1 exp2) = (strBinaryExpLChild pexp exp1) ++ " or " ++ (strBinaryExpRChild pexp exp2)


-- Int: indentation level
strStmt :: Int -> Stmt -> String
-- In a procedure body, each statement should start on a new line. So ++ newline in each's end
strStmt indentLevel (Assign lValue exp) = 
  -- <lvalue> <- <exp>;
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
  --     where <exp-list> is a (possibly empty according to parser) comma-separated list of expressions.
  (addIndentation indentLevel) ++ "call " ++ ident ++ surroundByParens (intercalate comma (map strExp exps)) ++ semicolon ++ newline
-- thenStmts is non-empty according to parser, elseStmts is possible empty according to parser
strStmt indentLevel (IfThen exp thenStmts) = 
  -- if <exp> then <stmt-list> fi
    -- "if ... then" should be printed on one line, irrespective of the size of the intervening expression
    (addIndentation indentLevel) ++ "if " ++ (strExp exp) ++ " then" ++ newline ++
    -- more indentation
    (concatMap (strStmt (indentLevel+1)) thenStmts) ++
    -- the terminating fi should be indented exactly as the corresponding if.
    (addIndentation indentLevel) ++ "fi" ++ newline
strStmt indentLevel (IfThenElse exp thenStmts elseStmts) = 
  -- if <expr> then <stmt-list> else <stmt-list> fi
    -- "if ... then" should be printed on one line, irrespective of the size of the intervening expression
    (addIndentation indentLevel) ++ "if " ++ (strExp exp) ++ " then" ++ newline ++
    -- more indentation
    (concatMap (strStmt (indentLevel+1)) thenStmts) ++
    (addIndentation indentLevel) ++ "else" ++ newline ++
    -- more indentation
    (concatMap (strStmt (indentLevel+1)) elseStmts) ++
    -- the terminating fi and else should be indented exactly as the corresponding if.
    (addIndentation indentLevel) ++ "fi" ++ newline
-- stmts is non-empty according to parser
strStmt indentLevel (While exp stmts) = 
  -- In a while statement, "while ... do" should be printed on one line, 
  --    irrespective of the size of the intervening expression
  (addIndentation indentLevel) ++ "while " ++ (strExp exp) ++ " do" ++ newline ++
  -- more indentation
  (concatMap (strStmt (indentLevel+1)) stmts) ++
  -- The terminating od should be indented exactly as the corresponding while.
  (addIndentation indentLevel) ++ "od" ++ newline

-----------------------------------------------------------------
--  Record toString function 
-----------------------------------------------------------------
-- field declaration is of:
--   1. boolean or integer
--   2. followed by an identifier (the field name).
strFieldDecl :: FieldDecl -> String
strFieldDecl (FieldDecl baseType fieldName) = (strBaseType baseType) ++ " " ++ fieldName

-- non-empty input list fieldDecls@(x:xs) according to parser
strFieldDecls :: [FieldDecl] -> String
-- first field decl starts with {
strFieldDecls (x:xs) = (addIndentation 1) ++ "{ " ++ (strFieldDecl x) ++ newline ++
-- rest start with ;
                       (concatMap 
                          (\y -> (addIndentation 1) ++ "; " ++ (strFieldDecl y) ++ newline) 
                          xs
                       )

-- convert record to string
-- A record type definition involving n fields should be printed on n + 2 lines, 
-- as follows:
--     1. The first line contains the word record. 
--     2. The remaining lines should be indented, with the first n containing one field declaration each 
--        (the first preceded by a left brace and a single space, the rest preceded 
--          by a semicolon and a single space), 
--          see above strFieldDecls
--     3. and with the last line containing the record name, preceded by a right 
--          brace and a single space, and followed by a semicolon;
strRecord :: Record -> String
strRecord (Record fieldDecls recordName) = 
  "record" ++ newline ++
  (strFieldDecls fieldDecls) ++ 
  (addIndentation 1) ++ "} " ++ recordName ++ semicolon ++ newline


-----------------------------------------------------------------
--  Array toString function 
-----------------------------------------------------------------
-- An array type definition should be printed on a single line. 
-- It contains the word array, 
-- followed by a positive integer in square brackets all without intervening 
--      white space.
-- That string, the type, and the type alias, should be separated by single 
--    spaces, and the whole line terminated by a semicolon.
strArray :: Array -> String
strArray (Array arraySize arrayType arrayName) = 
  "array" ++ surroundByBrackets (show arraySize) ++ " " ++ (strDataType arrayType) ++ " " ++ arrayName ++ semicolon ++ newline


-----------------------------------------------------------------
--  Procedure toString functions 
-----------------------------------------------------------------
strParameter :: Parameter -> String
strParameter (DataParameter dataType paraName) = (strDataType dataType) ++ " " ++ paraName
strParameter (BooleanVal paraName) = "boolean val " ++ paraName
strParameter (IntegerVal paraName) = "integer val " ++ paraName

-- The procedure head (that is, the keyword, procedure name, and list of formal 
--     parameters) should be on a single line.
strProcedureHeader :: ProcedureHeader -> String
strProcedureHeader (ProcedureHeader procedureName parameters) = 
  procedureName ++ " " ++ (surroundByParens (intercalate comma (map strParameter parameters)))

-- A variable declaration consists of
--   a) a type name (boolean, integer, or a type alias),
--   b) followed by a 1+ comma-separated list of 
--        identifiers,
--     i)  the list terminated with a semicolon.
--     ii) There may be any number of variable declarations, in any order.
strVariableDecl :: VariableDecl -> String
strVariableDecl (VariableDecl dataType varNames) = 
  -- Within each procedure, declarations and top-level statements should be indented.
  (addIndentation 1) ++ (strDataType dataType) ++ " " ++ (intercalate comma varNames)  ++ semicolon ++
  -- Each variable declaration should be on a separate line.
  newline

-- variableDecls can be empty according to parser
-- stmts is non-empty according to parser
strProcedureBody :: ProcedureBody -> String
strProcedureBody (ProcedureBody variableDecls stmts) = 
  (concatMap strVariableDecl variableDecls) ++
  -- The { and } that surround a procedure body should begin at the start of a 
  --    line (no indentation). 
  -- Moreover, these delimiters should appear alone, each making up a single line.
  "{" ++ newline ++ 
  -- Within each procedure, declarations and top-level statements should be indented.
  concatMap (strStmt 1) stmts ++ 
  "}" ++ newline

-- convert procedure to string
strProcedure :: Procedure -> String
-- The keyword procedure should begin at the start of a line (no indentation)
-- The procedure head (that is, the keyword, procedure name, and list of formal 
--     parameters) should be on a single line.
strProcedure (Procedure ph pb) =
  "procedure " ++ (strProcedureHeader ph) ++ newline ++
  (strProcedureBody pb)


-----------------------------------------------------------------
--  Program toString function 
-----------------------------------------------------------------
strProgram :: Program -> String
-- If there are no record and array type definitions, the first procedure should 
--    start on line 1.
strProgram (Program [] [] procedures) = intercalate newline (map strProcedure procedures)
-- Otherwise there should be a single blank line between the type definitions 
--    and the first procedure.
strProgram (Program records arraies procedures) = 
  -- Each type definition should start on a new line, and there should be no 
  --    blank lines between type definitions. So below two has no newline in between
  concatMap strRecord records ++
  concatMap strArray arraies ++
  newline ++ 
  -- Consecutive procedure definitions should be separated by a single blank line.
  intercalate newline (map strProcedure procedures)
