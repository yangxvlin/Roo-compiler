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
     , Q.identLetter     = alphaNum <|> char '_' <|> char '\''
     , Q.opStart         = oneOf "+-*<"
     , Q.opLetter        = oneOf "="
     , Q.reservedNames   = joeyReserved
     , Q.reservedOpNames = joeyOpnames
     })

whiteSpace    = Q.whiteSpace scanner
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

joeyReserved, joeyOpnames :: [String]

-- reserved words according to the specification
joeyReserved
  = ["and", "array", "boolean", "call", "do", "else", "false", "fi", "if", 
    "integer", "not", "od", "or", "procedure", "read", "record", "then", 
    "true", "val", "while", "write", "writeln"]

-- reserved operators from specification
-- 12 binary oprator (and, or above); 
-- 2 unary: not (above), -; 
-- assignment operator <-
joeyOpnames 
  = [ "+", "-", "*", "/", "=", "!=", "<", "<=", ">", ">=", "<-"]

pBaseType :: Parser BaseType
pBaseType
  = do { reserved "boolean"; return BooleanType }
    <|>
    do { reserved "integer"; return IntegerType }
    <?>
      "base type"

-----------------------------------------------------------------
--  pLiterals
-----------------------------------------------------------------
pBooleanLiteral :: Parser BooleanLiteral
pBooleanLiteral
 = do { reserved "true"; return (True) }
   <|>
   do { reserved "false"; return (False) }
   <?>
      "boolean literal"

pIntegerLiteral :: Parser IntegerLiteral
pIntegerLiteral
  = do
      n <- natural <?> "number"
      return (fromInteger n :: Int)
    <?>
      "Integer Literal"

pStringLiteral :: Parser StringLiteral
pStringLiteral
  =
    do
      s <- stringLiteral
      return (s)
    <?>
      "string literal"

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
-- An lvalue (<lvalue>) has four (and only four) possible forms:
-- <id> [ <exp> ] . <id>
-- <id> [ <exp> ]
-- <id> . <id>
-- <id>
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
--  Expression related parsers

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
            pneg
            -- pnot
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
      "bool const"

pIntConst
  = 
    do
      i <- pIntegerLiteral
      return (IntConst i)
    <?>
      "int const"

pStrConst
  = 
    do
      s <- pStringLiteral
      return (StrConst s)
    <?>
      "string literal"

pneg :: Parser Exp
pneg
  =
  do
    reservedOp "-"
    exp <- pExp
    return (Op_neg exp)
  <?>
    "negation"

pnot :: Parser Exp
pnot 
  =
    do
      reserved "not"
      exp <- pExp
      return (Op_not exp)
    <?>
      "not expression"

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
--     if <exp> then <stmt-list> fi # just make second [Stmt] emoty
--     while <expr> do <stmt-list> od
--         where <stmt-list> is a non-empty sequence of statements, atomic or composite
pStmt = choice [pStmtAtom, pStmtComp]

pStmtAtom
  = 
    do
      r <- choice [pAsg, pRead, pWrite, pWriteln, pCall]
      semi
      return r
    <?>
      "atomic statement"

pAsg, pRead, pWrite, pWriteln, pCall :: Parser Stmt
-- <lvalue> <- <exp> ;
pAsg
  = do
      -- Recognise the left value
      lvalue <- pLValue
      -- Reserved operator "<-"
      reservedOp "<-"
      -- Parse the assigned expression
      rvalue <- pExp
      -- Assign statement ends with semicolon ";"
      return (Assign lvalue rvalue)
    <?>
      "assign"

pRead
  = do 
      reserved "read"
      lvalue <- pLValue
      return (Read lvalue)
    <?>
      "read"

-- TODO can we write write/writeln with a helper function for code duplication?
pWrite
  = do 
      reserved "write"
      -- TODO can we  have string literal here?
      expr <- pExp
      return (Write expr)
    <?>
      "write"

pWriteln
  = do 
      reserved "writeln"
      -- TODO can we  have string literal here?
      expr <- pExp
      return (Writeln expr)
    <?>
      "writeln"

pCall
  = do
      reserved "call"
      ident <- identifier
      exprs <- parens (pExp `sepBy` comma)
      return (Call ident exprs)
    <?>
      "call"

pStmtComp = (choice [pIf, pWhile]) <?> "composite statement"

pIf, pWhile :: Parser Stmt
pIf
  = do
      
      reserved "if"
      exp <- pExp
      reserved "then"
      stmts <- many1 pStmt
      -- check if there is an else statment
      -- if not, return empty
      estmts <- (
        do
          reserved "fi"
          return []
        <|>
        do
          reserved "else"
          -- else body can not be empty
          s <- many1 pStmt
          reserved "fi"
          return s
        )
      return (If exp stmts estmts)
    <?>
      "if"

pWhile
  = do
      reserved "while"
      exp <- pExp
      reserved "do"
      stmts <- many1 pStmt
      reserved "od"
      return (While exp stmts)
    <?>
      "while"

-----------------------------------------------------------------
--  Procedure related parser
-----------------------------------------------------------------
-- Each formal parameter has two components (in the given order):
-- 1. a parameter type/mode indicator, which is one of these five:
--   a) a type alias and an identifier,
--   b) boolean,
--   c) integer,
--   d) boolean and an identifier
--   e) integer and an identifier
pParameter :: Parser Parameter
pParameter
  =
    try(
      do 
        reserved "boolean"
        reserved "val"
        name <- identifier
        return (BooleanVal name)
    )
    <|>
    try(
      do 
        reserved "integer"
        reserved "val"
        name <- identifier
        return (IntegerVal name)
    )
    <|>
    -- do
    --   -- parse integer literal
    --   integer <- pIntegerLiteral
    --   return (IntegerParameter integer)
    -- <|>
    -- do
    --   -- parse boolean literal
    --   boolean <- pBooleanLiteral
    --   return (BooleanParameter boolean)
    -- <|>
    do
      -- parse boolean/integer/type_alias variable
      paraType <- pDataType
      name <- identifier
      return (DataParameter paraType name)
    <?>
      "parameter"

-- The header has two components (in this order):
--   1. an identifier (the procedure's name), and
--   2. a comma-separated list of zero or more formal parameters within a pair 
--      of parentheses (so the parentheses are always present).
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
--   b) followed by a non-empty (enforced in parser) comma-separated list of 
  --    identifiers,
--     i)  the list terminated with a semicolon.
--     ii) There may be any number of variable declarations, in any order.
pVariable :: Parser VariableDecl
pVariable
  =
    do
      varType <- pDataType
      varNames <- (identifier `sepBy1` comma) --`endBy1` semi--    "var1, var2, var3;"
      semi
      return (VariableDecl varType varNames)
    <?>
      "variable"


-- procedure body consists of 0+ local variable declarations,
-- 1. A variable declaration consists of
--   a) a type name (boolean, integer, or a type alias),
--   b) followed by a non-empty comma-separated list of identifiers,
--     i)  the list terminated with a semicolon.
--     ii) There may be any number of variable declarations, in any order.
-- 2. followed by a non-empty (enforced in parser) sequence of statements,
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
      -- need to check arraySize > 0
      arraySize <- brackets pIntegerLiteral
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
--   2. a non-empty (enforced in parser) list of field declarations, separated 
--      by semicolons, the whole list enclosed in braces,
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
--  pProgram is the topmost parsing function. It looks for a 
--  header "procedure main()", followed by the program body.
-----------------------------------------------------------------
-- A Roo program consists of 
--   1. zero or more record type definitions, followed by 
--   2. zero or more array type definitions, followed by 
--   3. one or more (enforced in parser) procedure definitions.
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
-- main
-----------------------------------------------------------------

joeyParse :: Parser Program
joeyParse
  = do
      whiteSpace
      p <- pProgram
      eof
      return p

ast :: String -> Either ParseError Program
ast input
  =  runParser joeyParse 0 "" input

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

