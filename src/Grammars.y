{
module Grammars where

import Lex (Token(..),lexer)

}

%name parse
%tokentype { Token }
%error { parseError }

%token
    var    { TokenId $$ }
    int    { TokenNum $$ }
    bool    { TokenBool $$ }
    '+'    { TokenSuma }
    '-'    { TokenResta }
    '*'    { TokenMult }
    '/'    { TokenDiv }
    add1    { TokenAdd1 }
    sub1    { TokenSub1 }
    sqrt    { TokenSqrt }
    expt    { TokenExpt }
    '='    { TokenEq }
    '!='    { TokenNeq }
    '<'    { TokenLt }
    "<="    { TokenLeq }
    '>'    { TokenGt }
    '>='    { TokenGeq }
    "not"    { TokenNot }
    '('    { TokenPA }
    ')'    { TokenPC }
    '['    { TokenCorcheteA }
    ']'    { TokenCorcheteC }
    ','    { TokenComa }
    let    { TokenLet }
    "let*"    { TokenLetStar } 
    letrec    { TokenLetRec }
    if0    { TokenIf0 }
    if    { TokenIf }
    cond    { TokenCond }
    else    { TokenElse }
    lambda    { TokenLambda }
    pair    { TokenPair }
    fst    { TokenFst }
    snd    { TokenSnd }
    head    { TokenHead }
    tail    { TokenTail }
    list    { TokenList }
    nil    { TokenNil }

%%

SASA : var    { IdS $1 }
    | int    { NumS $1 }
    | bool    { BooleanS $1 }
    | nil    { NilS }

    -- Operadores Aritméticos (Variádicos >= 2)
    | '(' '+'   ExprList2 ')'    { AddS $3 }
    | '(' '-'   ExprList2 ')'    { SubS $3 }
    | '(' '*'   ExprList2 ')'    { MulS $3 }
    | '(' '/'   ExprList2 ')'    { DivS $3 }
    | '(' add1  SASA ')'    { Add1S $3 }
    | '(' sub1  SASA ')'    { Sub1S $3 }
    | '(' sqrt  SASA ')'    { SqrtS $3 }
    | '(' expt  SASA SASA ')'    { ExptS $3 $4 }

    -- Operadores Relacionales (Variádicos >= 2)
    | '(' '='   ExprList2 ')'    { EqS $3 }
    | '(' '!='  ExprList2 ')'    { NeqS $3 }
    | '(' '<'   ExprList2 ')'    { LtS $3 }
    | '(' "<="  ExprList2 ')'    { LeqS $3 }
    | '(' '>'   ExprList2 ')'    { GtS $3 }
    | '(' '>='  ExprList2 ')'    { GeqS $3 }

    -- Lógica y Condicionales
    | '(' "not" SASA ')'    { NotS $3 }
    | '(' if0 SASA SASA SASA ')'    { IfS (EqS [$3, NumS 0]) $4 $5 }
    | '(' if  SASA SASA SASA ')'    { IfS  $3 $4 $5 }
    | '(' cond CondClauses ElseClause ')' { CondS $3 $4 }

    -- Listas
    | '[' ExprListComma ']'    { ListS $2 }

    -- Pares y operaciones de Listas/Pares
    | '(' pair SASA SASA ')'    { PairS $3 $4 }
    | '(' fst  SASA ')'    { FstS $3 }
    | '(' snd  SASA ')'    { SndS $3 }
    | '(' head SASA ')'    { HeadS $3 }
    | '(' tail SASA ')'    { TailS $3 }

    -- Enlaces (let, let* variádicos)
    | '(' let    '(' BindingList ')' SASA ')'    { LetS    $4 $6 }
    | '(' "let*" '(' BindingList ')' SASA ')'    { LetStarS $4 $6 } 
        -- letrec usand el Z Combinator 
    | '(' letrec '(' var SASA ')' SASA ')'    { LetRecS $4 $5 $7 }

        -- Funciones y Aplicación (Variádicas)
    | '(' lambda '(' VarList ')' SASA ')'    { FunS $4 $6 }
    | '(' SASA ExprList ')'    { AppS $2 $3 }

-- Reglas auxiliares para listas



-- Lista de 2 o más expresiones (para operadores variádicos)
ExprList2 : SASA SASA            { [$1, $2] }
          | ExprList2 SASA       { $1 ++ [$2] }

-- Lista de 0 o más expresiones (para aplicación AppS)
ExprList : {- empty -}           { [] }
         | ExprList SASA         { $1 ++ [$2] }

-- Lista de 0 o más expresiones separadas por coma
ExprListComma : {- empty -}      { [] }
              | SASA             { [$1] }
              | ExprListComma ',' SASA { $1 ++ [$3] }

-- Lista de Bindings para let/let*
BindingList : Binding            { [$1] }
            | BindingList Binding { $1 ++ [$2] }

Binding : '(' var SASA ')'       { ($2, $3) }

-- Lista de 0 o más variables (para lambda)
VarList : {- empty -}            { [] }
        | VarList var            { $1 ++ [$2] }

-- Cláusulas para cond
CondClauses : CondClause         { [$1] }
            | CondClauses CondClause { $1 ++ [$2] }

CondClause : '[' SASA SASA ']'   { ($2, $3) }

ElseClause : '[' else SASA ']'   { $3 }


{

parseError :: [Token] -> a
parseError t = error ("Parse error: " ++ show t)

-- Sintaxis Abstracta de Superficie (SASA)
data SASA = IdS String
    | NumS Int
    | BooleanS Bool
    | NilS
    | AddS [SASA]
    | SubS [SASA]
    | MulS [SASA]
    | DivS [SASA]
    | LeqS [SASA]
    | EqS [SASA]
    | NeqS [SASA]
    | LtS [SASA]
    | GtS [SASA]
    | GeqS [SASA]
    -- Operadores unarios/binarios fijos
    | Add1S SASA
    | Sub1S SASA
    | SqrtS SASA
    | ExptS SASA SASA
    | NotS SASA
    -- Enlaces
    | LetS [(String, SASA)] SASA
    | LetStarS [(String, SASA)] SASA
    | LetRecS String SASA SASA
    -- Condicionales
    | If0S SASA SASA SASA
    | IfS  SASA SASA SASA
    | CondS [(SASA, SASA)] SASA
    | FunS [String] SASA
    | AppS SASA [SASA]
    -- Pares y Listas
    | PairS SASA SASA
    | FstS SASA
    | SndS SASA
    | ListS [SASA]
    | HeadS SASA
    | TailS SASA
    deriving(Show)
}