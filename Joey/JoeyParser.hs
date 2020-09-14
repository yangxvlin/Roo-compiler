module JoeyParser (ast)
where 
import JoeyAST
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
     , Q.opStart         = oneOf "+-*<"
     , Q.opLetter        = oneOf "-"
     , Q.reservedNames   = joeyReserved
     , Q.reservedOpNames = joeyOpnames
     })

whiteSpace = Q.whiteSpace scanner
natural    = Q.natural scanner
identifier = Q.identifier scanner
semi       = Q.semi scanner
comma      = Q.comma scanner
dot        = Q.dot scanner
parens     = Q.parens scanner
braces     = Q.braces scanner
squares    = Q.squares scanner
reserved   = Q.reserved scanner
reservedOp = Q.reservedOp scanner

joeyReserved, joeyOpnames :: [String]

joeyReserved
  = ["false", "do", "else", "fi", "if", "main", "od", "procedure"
    , "read", "then", "true", "while", "write", "writeln"
    ]

joeyOpnames 
  = ["+", "-", "*", "<-"]

-----------------------------------------------------------------
--  pExp parses expressions. It is built using Parces's powerful
--  buildExpressionParser and takes into account the operator
--  precedences and associativity specified in 'opTable' below.
-----------------------------------------------------------------

pExp :: Parser Exp
pExp
  = buildExpressionParser opTable pFac
    <?> "expression"

pFac :: Parser Exp
pFac
  = choice [parens pExp, pNum, pBool, pIdent]
    <?> "simple expression"

opTable
  = [ [ binary "*" Op_mul ]
    , [ binary "+" Op_add
      , binary "-" Op_sub
      ]
    ]

binary name op
  = Infix (do { reservedOp name
              ; return (BinOpExp op)
              }
          ) AssocLeft

-----------------------------------------------------------------
--  pProgram is the topmost parsing function. It looks for a 
--  header "procedure main()", followed by the program body.
-----------------------------------------------------------------

pProgram :: Parser Program
pProgram
  = do
      reserved "procedure"
      reserved "main"
      parens (return ())
      (decls,stmts) <- pProgBody
      return (Program decls stmts)
      
-----------------------------------------------------------------
--  pProgBody looks for a sequence of declarations followed by a
--  sequence of statements.
-----------------------------------------------------------------

pProgBody :: Parser ([Decl],[Stmt])
pProgBody
  = do
      decls <- many pDecl
      stmts <- braces (many1 pStmt)
      return (decls,stmts)

pDecl :: Parser Decl
pDecl
  = do
      typeName <- pTypeName
      ident <- identifier
      semi
      return (Decl typeName ident)

pTypeName :: Parser TypeName
pTypeName
  = do { reserved "boolean"; return BoolType }
    <|>
    do { reserved "integer"; return IntType }
      
-----------------------------------------------------------------
--  pStmt is the main parser for statements. Joey only has read
--  and write statements, and assignments.
-----------------------------------------------------------------

pStmt, pRead, pWrite, pAsg :: Parser Stmt

pStmt 
  = choice [pAsg, pRead, pWrite]

pAsg
  = do
      lvalue <- pLValue
      reservedOp "<-"
      expr <- pExp
      semi
      return (Assign lvalue expr)

pRead
  = do 
      reserved "read"
      lvalue <- pLValue
      semi
      return (Read lvalue)

pWrite
  = do 
      reserved "write"
      expr <- (pString <|> pExp)
      semi
      return (Write expr)

-----------------------------------------------------------------
--  The following are parsers for strings, numbers, booleans, 
--  identifiers (variable names are the only lvalues in Joey).
-----------------------------------------------------------------

pString, pNum, pBool, pIdent :: Parser Exp

pString 
  = do
      char '"'
      str <- many (satisfy (/= '"'))
      char '"'
      whiteSpace
      return (StrConst str)
    <?>
    "string"

pNum
  = do
      n <- natural <?> ""
      return (IntConst (fromInteger n :: Int))
    <?>
    "number"

pBool
  = do { reserved "true"; return (BoolConst True) }
    <|>
    do { reserved "false"; return (BoolConst False) }

pIdent 
  = do
      lval <- pLValue
      return (Lval lval)
    <?>
    "identifier"

pLValue :: Parser LValue
pLValue 
  = do
      ident <- identifier
      return (LId ident)
    <?>
    "lvalue"
      
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

