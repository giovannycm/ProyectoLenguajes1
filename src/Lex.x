{
module Lex (Token(..), lexer) where
import Data.Char (isSpace)
}

%wrapper "basic"


$white = [\x20\x09\x0C\x0B] -- ' ', tab, FF, VT
$newline = [\x0A\x0D]    -- LF, CR
$digit = 0-9
$letter = [A-Za-z_]
$idrest = [A-Za-z0-9_]

tokens :-

  -- Ignorar cualquier secuencia de espacios en blanco (que no sean newline)
  $white+    ;
  $newline+    ;

  ";" [^\n]* ;

  \(    { \_ -> TokenPA }
  \)    { \_ -> TokenPC }
  \[    { \_ -> TokenCorcheteA }
  \]    { \_ -> TokenCorcheteC }
  \,    { \_ -> TokenComa }

  -- Operadores Aritméticos
  \+    { \_ -> TokenSuma }
  \-    { \_ -> TokenResta }
  \*    { \_ -> TokenMult }
  \/    { \_ -> TokenDiv }
  add1    { \_ -> TokenAdd1 }
  sub1    { \_ -> TokenSub1 }
  sqrt    { \_ -> TokenSqrt }
  expt    { \_ -> TokenExpt }

-- Operadores Relacionales
  =    { \_ -> TokenEq }
  !=    { \_ -> TokenNeq }
  \<    { \_ -> TokenLt }
  \<=    { \_ -> TokenLeq }
  \>    { \_ -> TokenGt }
  \>=    { \_ -> TokenGeq }

  -- Operadores Lógicos/Booleanos
  not    { \_ -> TokenNot }
  
  -- Pares y Listas
  pair    { \_ -> TokenPair }
  fst    { \_ -> TokenFst }
  snd    { \_ -> TokenSnd }
  head    { \_ -> TokenHead }
  tail    { \_ -> TokenTail }
  list    { \_ -> TokenList }
  nil    { \_ -> TokenNil }

  -- Palabras Reservadas
  letrec    { \_ -> TokenLetRec }
  let    { \_ -> TokenLet }
  let\*    { \_ -> TokenLetStar }
  lambda    { \_ -> TokenLambda }
  if0    { \_ -> TokenIf0 }
  if    { \_ -> TokenIf }
  cond    { \_ -> TokenCond }
  else    { \_ -> TokenElse }

  -- Átomos
  "#t"    { \_ -> TokenBool True }
  "#f"    { \_ -> TokenBool False }
  0       { \s -> TokenNum 0 }
  [1-9]$digit*  { \s -> TokenNum (read s) }
  $letter$idrest* { \s -> TokenId s }

  -- Catch-all
  .    { \s -> error ("Lexical error: caracter no reconocido = " ++ show s ++ " | codepoints = " ++ show (map fromEnum s)) }

{
data Token
  = TokenId String
  | TokenNum Int
  | TokenBool Bool
  | TokenSuma
  | TokenResta
  | TokenMult
  | TokenDiv
  | TokenAdd1
  | TokenSub1
  | TokenSqrt
  | TokenExpt
  | TokenEq
  | TokenNeq
  | TokenLt
  | TokenLeq
  | TokenGt
  | TokenGeq
  | TokenNot
  | TokenPA
  | TokenPC
  | TokenCorcheteA
  | TokenCorcheteC
  | TokenComa
  | TokenLet
  | TokenLetStar
  | TokenLetRec
  | TokenIf0
  | TokenIf
  | TokenCond
  | TokenElse
  | TokenLambda
  | TokenPair
  | TokenFst
  | TokenSnd
  | TokenHead
  | TokenTail
  | TokenList
  | TokenNil
  deriving (Show, Eq)

-- Normalizar espacios: convertir todos los espacios en ' ' excepto newlines
normalizeSpaces :: String -> String
normalizeSpaces = map (\c -> if c == '\n' || c == '\r' then c else (if isSpace c then '\x20' else c))


lexer :: String -> [Token]
lexer = alexScanTokens . normalizeSpaces
}