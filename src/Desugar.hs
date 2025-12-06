module Desugar where

import Grammars (SASA(..))
import Data.List (foldl1)

-- AST Desugarizado (ASA)
data ASA
  = Id String
  | Num Int
  | Boolean Bool
  | Nil
  -- Operadores Binarios (Núcleo)
  | Add ASA ASA
  | Sub ASA ASA
  | Mul ASA ASA
  | Div ASA ASA
  | Expt ASA ASA
  | Eq ASA ASA
  | Neq ASA ASA
  | Lt ASA ASA
  | Leq ASA ASA
  | Gt ASA ASA
  | Geq ASA ASA
  -- Operadores Unarios (Núcleo)
  | Not ASA
  | Add1 ASA
  | Sub1 ASA
  | Sqrt ASA
  -- Pares y Listas (Núcleo)
  | Pair ASA ASA
  | Fst ASA
  | Snd ASA
  -- Condicionales (Núcleo)
  | If ASA ASA ASA
  -- Funciones (Núcleo)
  | Fun String ASA
  | App ASA ASA
  deriving (Show, Eq)

-- AST de valores
data ASAValues
  = IdV String
  | NumV Int
  | BooleanV Bool
  | NilV
  -- Operadores Binarios
  | AddV ASAValues ASAValues
  | SubV ASAValues ASAValues
  | MulV ASAValues ASAValues
  | DivV ASAValues ASAValues
  | ExptV ASAValues ASAValues
  | EqV ASAValues ASAValues
  | NeqV ASAValues ASAValues
  | LtV ASAValues ASAValues
  | LeqV ASAValues ASAValues
  | GtV ASAValues ASAValues
  | GeqV ASAValues ASAValues
  -- Operadores Unarios
  | NotV ASAValues
  | Add1V ASAValues
  | Sub1V ASAValues
  | SqrtV ASAValues
  -- Pares y Listas
  | PairV ASAValues ASAValues
  | FstV ASAValues
  | SndV ASAValues
  -- Condicionales
  | IfV ASAValues ASAValues ASAValues
  -- Funciones y Aplicación
  | FunV String ASAValues
  | ExprV ASAValues [(String, ASAValues)]
  | ClosureV String ASAValues [(String, ASAValues)]
  | AppV ASAValues ASAValues
  deriving(Show, Eq)

-- Función auxiliar para comparaciones encadenadas
-- (op e1 e2 e3) -> (if (op e1 e2) (op e2 e3) #f)
desugarChain :: (ASA -> ASA -> ASA) -> [SASA] -> ASA
desugarChain op [e1, e2] = op (desugar e1) (desugar e2)
desugarChain op (e1:e2:rest) = 
  If (op (desugar e1) (desugar e2)) 
     (desugarChain op (e2:rest)) 
     (Boolean False)
desugarChain _ _ = error "Operador relacional requiere al menos 2 argumentos"

-- Traducción de SASA a ASA
desugar :: SASA -> ASA
desugar (IdS i)         = Id i
desugar (NumS n)        = Num n
desugar (BooleanS b)    = Boolean b
desugar (NilS)          = Nil

-- Operadores Aritméticos Variádicos
desugar (AddS es)       = foldl1 Add (map desugar es)
desugar (SubS es)       = foldl1 Sub (map desugar es)
desugar (MulS es)       = foldl1 Mul (map desugar es)
desugar (DivS es)       = foldl1 Div (map desugar es)
desugar (ExptS e1 e2)   = Expt (desugar e1) (desugar e2)
desugar (Add1S e)       = Add1 (desugar e)
desugar (Sub1S e)       = Sub1 (desugar e)
desugar (SqrtS e)       = Sqrt (desugar e)

-- Operadores Relacionales Variádicos 
desugar (EqS es)        = desugarChain Eq es
desugar (NeqS es)       = desugarChain Neq es
desugar (LtS es)        = desugarChain Lt es
desugar (LeqS es)       = desugarChain Leq es
desugar (GtS es)        = desugarChain Gt es
desugar (GeqS es)       = desugarChain Geq es
desugar (NotS e)        = Not (desugar e)

-- Condicionales
desugar (If0S c t e)    = If (Eq (desugar c) (Num 0)) (desugar t) (desugar e)
desugar (IfS c t e)     = If (desugar c) (desugar t) (desugar e)

-- cond se desazucara a if anidados
desugar (CondS clauses elseE) = desugarCond clauses elseE
  where
    desugarCond [] elseExpr = desugar elseExpr
    desugarCond ((c, e):cs) elseExpr = If (desugar c) (desugar e) (desugarCond cs elseExpr)

-- Pares y Listas
desugar (PairS e1 e2)   = Pair (desugar e1) (desugar e2)
desugar (FstS e)        = Fst (desugar e)
desugar (SndS e)        = Snd (desugar e)

-- Listas: [e1, e2, e3] -> (pair e1 (pair e2 (pair e3 nil)))
desugar (ListS es)      = desugarList es
  where
    desugarList [] = Nil
    desugarList (x:xs) = Pair (desugar x) (desugarList xs)

desugar (HeadS e)       = Fst (desugar e)  
desugar (TailS e)       = Snd (desugar e) 

-- let variádico: (let ((x v1) (y v2)) c) -> ((lambda (x y) c) v1 v2)
desugar (LetS bindings c) = desugar (AppS (FunS (map fst bindings) c) (map snd bindings))

-- let* variádico: (let* ((x v1) (y v2)) c) -> (let ((x v1)) (let* ((y v2)) c))
desugar (LetStarS [] c)      = desugar c
desugar (LetStarS (b:bs) c)  = desugar (LetS [b] (LetStarS bs c))

-- letrec: (letrec (p v) c) -> (let ((p (Z (lambda (p) v)))) c)  
-- p identificador de la función
-- v valor de la función
-- c cuerpo de la función
desugar (LetRecS p v c) = desugar (LetS [(p, AppS (IdS "Z") [FunS [p] v])] c)

-- Funciones y Aplicación
-- lambda variádica: (lambda (p1 p2) c) -> (Fun p1 (Fun p2 c))
desugar (FunS params c) = desugarFun params c
  where
    desugarFun [] body = desugar body
    desugarFun (p:ps) body = Fun p (desugarFun ps body)

-- App variádica: (f a1 a2) -> (App (App f a1) a2)
desugar (AppS f args) = foldl App (desugar f) (map desugar args)

-- Traducción de ASA a ASAValues
desugarV :: ASA -> ASAValues
desugarV (Id i)         = IdV i
desugarV (Num n)        = NumV n
desugarV (Boolean b)    = BooleanV b
desugarV (Nil)          = NilV
desugarV (Add i d)      = AddV (desugarV i) (desugarV d)
desugarV (Sub i d)      = SubV (desugarV i) (desugarV d)
desugarV (Mul i d)      = MulV (desugarV i) (desugarV d)
desugarV (Div i d)      = DivV (desugarV i) (desugarV d)
desugarV (Expt i d)     = ExptV (desugarV i) (desugarV d)
desugarV (Eq i d)       = EqV (desugarV i) (desugarV d)
desugarV (Neq i d)      = NeqV (desugarV i) (desugarV d)
desugarV (Lt i d)       = LtV (desugarV i) (desugarV d)
desugarV (Leq i d)      = LeqV (desugarV i) (desugarV d)
desugarV (Gt i d)       = GtV (desugarV i) (desugarV d)
desugarV (Geq i d)      = GeqV (desugarV i) (desugarV d)
desugarV (Not e)        = NotV (desugarV e)
desugarV (Add1 e)       = Add1V (desugarV e)
desugarV (Sub1 e)       = Sub1V (desugarV e)
desugarV (Sqrt e)       = SqrtV (desugarV e)
desugarV (Pair i d)     = PairV (desugarV i) (desugarV d)
desugarV (Fst e)        = FstV (desugarV e)
desugarV (Snd e)        = SndV (desugarV e)
desugarV (If c t e)     = IfV (desugarV c) (desugarV t) (desugarV e)
desugarV (Fun p c)      = FunV p (desugarV c)
desugarV (App f a)      = AppV (desugarV f) (desugarV a)

--Z = \f.(\x.f(\v.((x x) v)))(\x.f(\v.((x x) v)))