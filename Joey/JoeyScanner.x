{
module Main where
}

%wrapper "basic"

$digit       = 0-9
@alpha       = [a-zA-Z]
@digits      = $digit+
@stringlit   = \" [^\"]* \"
@ident       = @alpha (@alpha | $digit | \_ | \')*
@comment     = \# [^\n]* \n

rules :-

  $white+    ;    -- skip white space
  @comment   ;    -- skip comments
  @digits    { \s -> INT_CONST (read s :: Int) }
  true       { \s -> BOOL_CONST True }
  false      { \s -> BOOL_CONST False }
  boolean    { \s -> BOOL }
  integer    { \s -> INT }
  procedure  { \s -> PROC }
  read       { \s -> READ }
  write      { \s -> WRITE }
  \<\-       { \s -> ASSIGN }
  \{         { \s -> LBRACE }
  \}         { \s -> RBRACE }
  \(         { \s -> LPAREN }
  \)         { \s -> RPAREN }
  \+         { \s -> PLUS }
  \-         { \s -> MINUS }
  \*         { \s -> MUL }
  \;         { \s -> SEMI }
  @ident     { \s -> IDENT s }
  @stringlit { \s -> LIT s }

{
data Token
  = INT_CONST Int | BOOL_CONST Bool | IDENT String | LIT String
  | BOOL | INT | PROC | READ | WRITE | ASSIGN | LBRACE | RBRACE
  | LPAREN | RPAREN | PLUS | MINUS | MUL | SEMI 
    deriving (Eq, Show)

main
  = do
      s <- getContents
      print (alexScanTokens s)
}

