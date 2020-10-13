-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Roo Compiler                      --
--  Implemented by Xulin Yang                            --
--  read from the bottom to top                          --
-----------------------------------------------------------
module RooParser (ast)
where 
import RooAST
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Q
import System.Environment
import System.Exit
import Debug.Trace (trace)

type Parser a
   = Parsec String Int a

scanner :: Q.TokenParser Int
scanner
   = Q.makeTokenParser
     (emptyDef
     { Q.commentLine     = "#"
     , Q.nestedComments  = True
     , Q.identStart      = letter
     -- An identifir is a non-empty sequence of alphanumeric characters, 
    --  underscore and apostrophe ('), and it must start with a (lower or upper case) letter.
     , Q.identLetter     = alphaNum <|> char '_' <|> char '\''
     , Q.opStart         = oneOf "+-*<"
     , Q.opLetter        = oneOf "="
     , Q.reservedNames   = rooReserved
     , Q.reservedOpNames = rooOpnames
     })

whiteSpace    = Q.whiteSpace scanner
lexeme        = Q.lexeme scanner
natural       = Q.natural scanner
identifier    = Q.identifier scanner
semi          = Q.semi scanner
comma         = Q.comma scanner
dot           = Q.dot scanner
parens        = Q.parens scanner
braces        = Q.braces scanner
brackets      = Q.brackets scanner
squares       = Q.squares scanner
reserved      = Q.reserved scanner
reservedOp    = Q.reservedOp scanner
stringLiteral = Q.stringLiteral scanner

rooReserved, rooOpnames :: [String]

-- reserved words according to the specification
rooReserved
  = ["and", "array", "boolean", "call", "do", "else", "false", "fi", "if", 
    "integer", "not", "od", "or", "procedure", "read", "record", "then", 
    "true", "val", "while", "write", "writeln"]

-- reserved operators from specification
-- 12 binary oprator (and, or; above reserved string); 
-- 2 unary: not (above reserved string), -; 
-- assignment operator <-
rooOpnames 
  = [ "+", "-", "*", "/", "=", "!=", "<", "<=", ">", ">=", "<-"]

-----------------------------------------------------------------
--  Note: 0+ is ensured using many/sepBy and 1+ using many1/sepBy1
-----------------------------------------------------------------

-----------------------------------------------------------------
--  parse reused base type integer/boolean; no string here as mentioned in AST
--  parse reused data type integer/boolean/type alias
-----------------------------------------------------------------
pBaseType :: Parser BaseType
pBaseType
  = do 
      reserved "boolean" 
      return BooleanType
    <|>
    do 
      reserved "integer"
      return IntegerType
    <?>
      "base type"

pDataType :: Parser DataType
pDataType
  =
    do
      baseType <- pBaseType
      return (BasyDataType baseType)
    <|>
    do
      alias <- identifier
      return (AliasDataType alias)
    <?>
      "data type"


-----------------------------------------------------------------
--  parse literals
-----------------------------------------------------------------
pBooleanLiteral :: Parser BooleanLiteral
pBooleanLiteral
 = 
   do { reserved "true"; return (True) }
   <|>
   do { reserved "false"; return (False) }
   <?>
      "boolean literal"

pIntegerLiteral :: Parser IntegerLiteral
pIntegerLiteral
  = 
    do
      n <- natural <?> "number"
      return (fromInteger n :: Int)
    <?>
      "Integer Literal"

-- don't accept newline, tab, quote but "\n", "\t", "\"" <- two character string should still be accepted
pcharacter :: Parser String
pcharacter
  =
    try(
      do
        string ('\\':['n'])
        return (['\\', 'n'])
    )
    <|>
    try(
      do
        string ('\\':['t'])
        return (['\\', 't'])
    )
    <|>
    try(
      do
        string ('\\':['"'])
        return (['\\', '"'])
    )
    <|>
    do
      c <- noneOf ['\n', '\t', '"']
      return ([c])
    <?>
      "any character except newline, tab, quote"

-- Parser for string
pString :: Parser String
pString
  =
    do
      -- String is surrounded by two quotes
      char '"'
      -- Parse characters except newline / tab characters and quotes
      str <- many pcharacter
      char '"' <?> "\'\"\' to wrap the string"
      whiteSpace -- consumes following spaces
      return (concat str)
    <?>
      "string cannot has newline, quote, tab"

pStringLiteral :: Parser StringLiteral
pStringLiteral
  =
    do
      s <- pString
      return (s)
    <?>
      "string literal"


-----------------------------------------------------------------
-- An lvalue (<lvalue>) has four (and only four) possible forms:
--     <id>
--     <id>.<id>
--     <id>[<exp>]
--     <id>[<exp>].<id>
-- An example lvalue is point[0].xCoord
-----------------------------------------------------------------
pLValue :: Parser LValue
pLValue 
  = try (
      do
        ident1 <- identifier
        exp <- brackets pExp
        dot
        ident2 <- identifier
        return (LBracketsDot ident1 exp ident2)
   ) 
    <|>
    try (
      do
        ident <- identifier
        exp <- brackets pExp
        return (LBrackets ident exp)
    )
    <|>
    try (
      do
        ident1 <- identifier
        dot
        ident2 <- identifier
        return (LDot ident1 ident2)
    )
    <|>
    do
      ident <- identifier
      return (LId ident)
    <?>
      "LValue"

-----------------------------------------------------------------
--  pExp is the main parser for expression. 
-- 
--  It is built using Parces's powerful
--  buildExpressionParser and takes into account the operator
--  precedences and associativity specified in 'opTable' below.
-----------------------------------------------------------------
prefix name fun
  = Prefix (do { reservedOp name
               ; return fun 
               }
           )

binary name op
  = Infix (do { reservedOp name
              ; return op
              }
          ) AssocLeft

relation name rel
  = Infix (do { reservedOp name
              ; return rel 
              }
          ) AssocNone

-- expression operators: 
--     All the operators on the same line have the same precedence, 
--         and the ones on later lines have lower precedence;
--     The six relational operators are non-associative 
--         so, for example, a = b = c is not a well-formed expression). 
--     The six remaining binary operators are left-associative.
-- -              |unary            |
-- * /            |binary and infix |left-associative
-- + -            |binary and infix |left-associative
-- = != < <= > >= |binary and infix |relational, non-associative 
-- not            |unary            |
-- and            |binary and infix |left-associative
-- or             |binary and infix |left-associative
opTable
  = [ [ prefix   "-"   Op_neg    ]
    , [ binary   "*"   Op_mul    , binary   "/"  Op_div  ]
    , [ binary   "+"   Op_add    , binary   "-"  Op_sub  ]
    , [ relation "="   Op_eq     , relation "!=" Op_neq  , relation "<"  Op_less
      , relation "<="  Op_less_eq, relation ">"  Op_large, relation ">=" Op_large_eq ]
    , [ prefix   "not" Op_not    ]
    , [ binary   "and" Op_and    ]
    , [ binary   "or"  Op_or     ]
    ]

pExp :: Parser Exp
pExp
  = buildExpressionParser opTable pFac
    <?> 
      "expression"

pFac :: Parser Exp
pFac
  = choice [parens pExp, -- ( <exp> ) 
            pLval,
            pBoolConst, 
            pIntConst, 
            pStrConst,
            pNeg         -- used to parse expression like ------1 (arbitrary unary minus before expression)
           ]
    <?> 
      "simple expression"

pLval, pBoolConst, pIntConst, pStrConst :: Parser Exp
pLval
  =
    do
      lval <- pLValue
      return (Lval lval)
    <?>
      "lval expression"

pBoolConst
  = 
    do
      b <- pBooleanLiteral
      return (BoolConst b)
    <?>
      "bool const/literal expression"

pIntConst
  = 
    do
      i <- pIntegerLiteral
      return (IntConst i)
    <?>
      "int const/literal expression"

pStrConst
  = 
    do
      s <- pStringLiteral
      return (StrConst s)
    <?>
      "string const/literal expression"

-- parse arbitrary unary minus in pFac
pNeg :: Parser Exp
pNeg
  =
  do
    reservedOp "-"
    exp <- pExp
    return (Op_neg exp)
  <?>
    "unary minus"


-----------------------------------------------------------------
--  pStmt is the main parser for statements. 
--  Statement related parsers
-----------------------------------------------------------------
pStmt, pStmtAtom, pStmtComp :: Parser Stmt
-- atom statement:
--     <lvalue> <- <exp> ;
--     read <lvalue> ;
--     write <exp> ;
--     writeln <exp> ;
--     call <id> ( <exp-list> ) ; 
--         where <exp-list> is a (possibly empty) comma-separated list of expressions.
-- composite statement:
--     if <expr> then <stmt-list> else <stmt-list> fi
--     if <exp> then <stmt-list> fi # just make second [Stmt] empty
--     while <expr> do <stmt-list> od
--         where <stmt-list> is a non-empty sequence of statements, atomic or composite
pStmt = choice [pStmtAtom, pStmtComp]

pStmtAtom
  = 
    do
      r <- choice [pAsg, pRead, pWrite, pWriteln, pCall]
      -- all atomic stmt's semicolon is comsumed here
      semi
      return r
    <?>
      "atomic statement"

pAsg, pRead, pWrite, pWriteln, pCall :: Parser Stmt

-- parse: <lvalue> <- <exp> ;
pAsg
  = 
    do
      lvalue <- pLValue
      reservedOp "<-"
      rvalue <- pExp
      return (Assign lvalue rvalue)
    <?>
      "assign"

-- parse: read <lvalue> ;
pRead
  = do 
      reserved "read"
      lvalue <- pLValue
      return (Read lvalue)
    <?>
      "read"

-- parse: write <exp> ;
pWrite
  = do 
      reserved "write"
      expr <- pExp
      return (Write expr)
    <?>
      "write"

-- parse: writeln <exp> ;
pWriteln
  = do 
      reserved "writeln"
      expr <- pExp
      return (Writeln expr)
    <?>
      "writeln"

-- parse: call <id>(<exp-list>) ; 
pCall
  = do
      reserved "call"
      ident <- identifier
      exprs <- parens (pExp `sepBy` comma) -- 0+ comma-separated list of expressions
      return (Call ident exprs)
    <?>
      "call"

pStmtComp = (choice [pIf, pWhile]) <?> "composite statement"

pIf, pWhile :: Parser Stmt

-- parse: 
--    if <exp> then <stmt-list> else <stmt-list> fi
--    if <exp> then <stmt-list> fi # just make above second [Stmt] empty
pIf
  = do
      
      reserved "if"
      exp <- pExp
      reserved "then"
      thenStmts <- many1 pStmt
      -- check if there is an else statment
      -- if not, return empty
      res <- (
        do
          reserved "fi"
          return (IfThen exp thenStmts)
        <|>
        do
          reserved "else"
          -- else body can not be empty
          elseStmts <- many1 pStmt
          reserved "fi"
          return (IfThenElse exp thenStmts elseStmts)
        )
      return res
    <?>
      "if"

-- parse: while <exp> do <stmt-list> od
pWhile
  = do
      reserved "while"
      exp <- pExp
      reserved "do"
      stmts <- many1 pStmt -- a 1+ sequence of statements, atomic or composite
      reserved "od"
      return (While exp stmts)
    <?>
      "while"

-----------------------------------------------------------------
--  Procedure related parser
-----------------------------------------------------------------
-- Each formal parameter has two components (in the given order):
--  1. a parameter type/mode indicator, which is one of these five:
--    a) a type alias,
--    b) boolean,
--    c) integer,
--    d) boolean val
--    e) integer val
--  2. an identifier
pParameter :: Parser Parameter
pParameter
  =
    try(
      do 
        -- parse boolean val variable
        reserved "boolean"
        reserved "val"
        name <- identifier
        return (BooleanVal name)
    )
    <|>
    try(
      do 
        -- parse integer val variable
        reserved "integer"
        reserved "val"
        name <- identifier
        return (IntegerVal name)
    )
    <|>
    do
      -- parse boolean/integer/type_alias variable
      paraType <- pDataType
      name <- identifier
      return (DataParameter paraType name)
    <?>
      "parameter"

-- The header has two components (in this order):
--   1. an identifier (the procedure's name), and
--   2. a comma-separated list of 0+ formal parameters within a pair 
--        of parentheses (so the parentheses are always present).
pProcedureHeader :: Parser ProcedureHeader
pProcedureHeader
  =
    do
      procedureName <- identifier
      parameters <- parens (pParameter `sepBy` comma)
      return (ProcedureHeader procedureName parameters)
    <?>
      "procedure header"

-- A variable declaration consists of
--   a) a type name (boolean, integer, or a type alias),
--   b) followed by a 1+ comma-separated list of 
--        identifiers,
--     i)  the list terminated with a semicolon.
--     ii) There may be any number of variable declarations, in any order.
pVariable :: Parser VariableDecl
pVariable
  =
    do
      varType <- pDataType
      varNames <- (identifier `sepBy1` comma)
      semi
      return (VariableDecl varType varNames)
    <?>
      "variable"


-- procedure body consists of 0+ local variable declarations,
--   1. A variable declaration consists of
--     a) a type name (boolean, integer, or a type alias),
--     b) followed by a 1+ comma-separated list of identifiers,
--       i)  the list terminated with a semicolon.
--       ii) There may be any number of variable declarations, in any order.
--   2. followed by a 1+ sequence of statements,
pProcedureBody :: Parser ProcedureBody
pProcedureBody
  =
    do
      vars <- many pVariable
      stmts <- braces (many1 pStmt)
      return (ProcedureBody vars stmts)
    <?>
      "procedure body"

-- Each procedure consists of (in the given order):
--   1. the keyword procedure,
--   2. a procedure header, and
--   3. a procedure body.
pProcedure :: Parser Procedure
pProcedure
  =
    do
      reserved "procedure"
      procedureHeader <- pProcedureHeader
      procedureBody <- pProcedureBody
      return (Procedure procedureHeader procedureBody)
    <?>
      "procedure"

-----------------------------------------------------------------
--  Array related parser
-----------------------------------------------------------------
-- array type definition consists of (in the given order):
--   1. the keyword array,
--   2. a (positive) integer literal enclosed in square brackets,
--   3. a type name which is either an identifier (a type alias) or one of 
--      boolean and integer,
--   4. an identifier (giving a name to the array type), and
--   5. a semicolon.
pArray :: Parser Array
pArray
  =
    do
      reserved "array"
      pos <- getPosition
      arraySize <- brackets pIntegerLiteral
      -- need to check arraySize > 0 (positive integer)
      if arraySize == 0
      then
        error ("array size sould not be 0 at line: " ++ (show (sourceLine pos)) ++ ", column: " ++ (show (sourceColumn pos + 1))) -- +1 to skip '['
      else do
        arrayType <- pDataType
        arrayName <- identifier
        semi
        return (Array arraySize arrayType arrayName)
      <?>
        "array"
        
-----------------------------------------------------------------
--  Record related parser
-----------------------------------------------------------------
-- field declaration is of:
--   1. boolean or integer
--   2. followed by an identifier (the field name).
pFieldDecl :: Parser FieldDecl
pFieldDecl
  =
    do
      fieldType <- pBaseType
      fieldName <- identifier
      return (FieldDecl fieldType fieldName)
    <?>
      "field declaration"

-- record consists of:
--   1. the keyword record,
--   2. a 1+ list of field declarations, separated by semicolons, 
--        the whole list enclosed in braces,
--   3. an identifier, and
--   4. a semicolon.
pRecord :: Parser Record
pRecord
  =
    do
      reserved "record"
      recordFieldDecls <- braces (pFieldDecl `sepBy1` semi)
      recordName <- identifier
      semi
      return (Record recordFieldDecls recordName)
    <?>
      "record"

-----------------------------------------------------------------
--  Program related parser
-----------------------------------------------------------------
-- A Roo program consists of 
--   1. 0+ record type definitions, followed by 
--   2. 0+ array type definitions, followed by 
--   3. 1+ procedure definitions.
pProgram :: Parser Program
pProgram
  = do
      records <- many pRecord
      arraies <- many pArray
      procedures <- many1 pProcedure
      return (Program records arraies procedures)
    <?>
      "program"


-----------------------------------------------------------------
-- main (given skeleton code)
-----------------------------------------------------------------

rooParse :: Parser Program
rooParse
  = do
      whiteSpace
      p <- pProgram
      eof
      return p

ast :: String -> Either ParseError Program
ast input
  =  runParser rooParse 0 "" input

pMain :: Parser Program
pMain
  = do
      whiteSpace
      p <- pProgram
      eof
      return p

main :: IO ()
main
  = do { progname <- getProgName
       ; args <- getArgs
       ; checkArgs progname args
       ; input <- readFile (head args)
       ; let output = runParser pMain 0 "" input
       ; case output of
           Right ast -> print ast
           Left  err -> do { putStr "Parse error at "
                           ; print err
                           }
       }

checkArgs :: String -> [String] -> IO ()
checkArgs _ [filename]
   = return ()
checkArgs progname _
   = do { putStrLn ("Usage: " ++ progname ++ " filename\n\n")
       ; exitWith (ExitFailure 1)
       }
