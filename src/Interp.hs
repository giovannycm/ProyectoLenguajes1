module Interp where

import Desugar

type Env = [(String, ASAValues)]

-- Reglas de paso pequeño

smallStep :: ASAValues -> Env -> (ASAValues, Env)
smallStep (IdV i) env    = (lookupEnv i env, env)
smallStep (NumV n) env    = (NumV n, env)
smallStep (BooleanV b) env    = (BooleanV b, env)
smallStep (NilV) env    = (NilV, env) 

-- Suma
smallStep (AddV (NumV n) (NumV m)) env    = (NumV (n + m), env)
smallStep (AddV (NumV n) d) env    = (AddV (NumV n) (fst (smallStep d env)), env)
smallStep (AddV i d) env    = (AddV (fst (smallStep i env)) d, env)

-- Resta
smallStep (SubV (NumV n) (NumV m)) env    = (NumV (n - m), env)
smallStep (SubV (NumV n) d) env    = (SubV (NumV n) (fst (smallStep d env)), env)
smallStep (SubV i d) env    = (SubV (fst (smallStep i env)) d, env)

-- Multiplicación
smallStep (MulV (NumV n) (NumV m)) env    = (NumV (n * m), env)
smallStep (MulV (NumV n) d) env    = (MulV (NumV n) (fst (smallStep d env)), env)
smallStep (MulV i d) env    = (MulV (fst (smallStep i env)) d, env)

-- División
smallStep (DivV (NumV n) (NumV m)) env
  | m == 0    = error "Error: División por cero"
  | otherwise = (NumV (n `div` m), env)
smallStep (DivV (NumV n) d) env    = (DivV (NumV n) (fst (smallStep d env)), env)
smallStep (DivV i d) env    = (DivV (fst (smallStep i env)) d, env)

-- Potencia
smallStep (ExptV (NumV n) (NumV m)) env    = (NumV (n ^ m), env)
smallStep (ExptV (NumV n) d) env    = (ExptV (NumV n) (fst (smallStep d env)), env)
smallStep (ExptV i d) env    = (ExptV (fst (smallStep i env)) d, env)

-- Comparaciones (NumV)
smallStep (LeqV (NumV n) (NumV m)) env    = (BooleanV (n <= m), env)
smallStep (LeqV (NumV n) d) env    = (LeqV (NumV n) (fst (smallStep d env)), env)
smallStep (LeqV i d) env    = (LeqV (fst (smallStep i env)) d, env)

smallStep (LtV (NumV n) (NumV m)) env    = (BooleanV (n < m), env)
smallStep (LtV (NumV n) d) env    = (LtV (NumV n) (fst (smallStep d env)), env)
smallStep (LtV i d) env    = (LtV (fst (smallStep i env)) d, env)

smallStep (GtV (NumV n) (NumV m)) env    = (BooleanV (n > m), env)
smallStep (GtV (NumV n) d) env    = (GtV (NumV n) (fst (smallStep d env)), env)
smallStep (GtV i d) env    = (GtV (fst (smallStep i env)) d, env)

smallStep (GeqV (NumV n) (NumV m)) env    = (BooleanV (n >= m), env)
smallStep (GeqV (NumV n) d) env    = (GeqV (NumV n) (fst (smallStep d env)), env)
smallStep (GeqV i d) env    = (GeqV (fst (smallStep i env)) d, env)

-- Comparaciones (Generales, para Eq/Neq)
smallStep (EqV v1 v2) env
  | isValueV v1 && isValueV v2 = (BooleanV (v1 == v2), env) 
  | isValueV v1 = (EqV v1 (fst (smallStep v2 env)), env)
  | otherwise   = (EqV (fst (smallStep v1 env)) v2, env)

smallStep (NeqV v1 v2) env
  | isValueV v1 && isValueV v2 = (BooleanV (v1 /= v2), env) 
  | isValueV v1 = (NeqV v1 (fst (smallStep v2 env)), env)
  | otherwise   = (NeqV (fst (smallStep v1 env)) v2, env)

-- Unarios
smallStep (NotV (BooleanV b))   env    = (BooleanV (not b), env)
smallStep (NotV b) env    = (NotV (fst (smallStep b env)), env)

smallStep (Add1V (NumV n)) env    = (NumV (n + 1), env)
smallStep (Add1V e) env    = (Add1V (fst (smallStep e env)), env)

smallStep (Sub1V (NumV n)) env    = (NumV (n - 1), env)
smallStep (Sub1V e) env    = (Sub1V (fst (smallStep e env)), env)

smallStep (SqrtV (NumV n)) env
  | n < 0    = error "Error: Raíz cuadrada de número negativo"
  | otherwise = (NumV (floor (sqrt (fromIntegral n :: Double))), env)
smallStep (SqrtV e) env    = (SqrtV (fst (smallStep e env)), env)

-- If (maneja tanto if como if0 desazucarado)
smallStep (IfV (BooleanV True)  t e) env = (t, env)
smallStep (IfV (BooleanV False) t e) env = (e, env)
smallStep (IfV c t e) env    = (IfV (fst (smallStep c env)) t e, env)

-- Pares y Listas
smallStep (PairV v1 v2) env
  | isValueV v1 && isValueV v2 = (PairV v1 v2, env) 
  | isValueV v1 = (PairV v1 (fst (smallStep v2 env)), env)
  | otherwise   = (PairV (fst (smallStep v1 env)) v2, env)

smallStep (FstV (PairV v1 v2)) env
  | isValueV v1 && isValueV v2 = (v1, env)
smallStep (FstV e) env    = (FstV (fst (smallStep e env)), env)

smallStep (SndV (PairV v1 v2)) env
  | isValueV v1 && isValueV v2 = (v2, env)
smallStep (SndV e) env    = (SndV (fst (smallStep e env)), env)

-- Head y Tail son desazucarados a Fst y Snd
smallStep (HeadV e) env    = smallStep (FstV e) env
smallStep (TailV e) env    = smallStep (SndV e) env

-- Funciones y aplicación
smallStep (FunV p c) env    = (ClosureV p c env, env)
smallStep (AppV cv@(ClosureV p b env') a) env
  | isValueV a = (ExprV b ((p, a) : env'), env)
  | otherwise = (AppV cv (fst (smallStep a env)), env)
smallStep (AppV f a) env    = (AppV (fst (smallStep f env)) a, env)

smallStep (ExprV v env') env
  | isValueV v = (v, env)
  | otherwise = (ExprV e1 env', env)
  where (e1, _) = smallStep v env'

-- Evaluador completo (repetir paso pequeño hasta valor)
interp :: ASAValues -> Env -> ASAValues
interp e env
  | isValueV e = e
  | otherwise  =
    let (e', env') = smallStep e env
    in interp e' env'

-- Predicados y auxiliares
isValueV :: ASAValues -> Bool
isValueV (NumV _)    = True
isValueV (BooleanV _)    = True
isValueV (ClosureV _ _ _)   = True
isValueV (NilV)    = True
isValueV (PairV v1 v2)    = isValueV v1 && isValueV v2
isValueV _    = False

lookupEnv :: String -> Env -> ASAValues
lookupEnv i [] = error ("Variable " ++ i ++ " not found")
lookupEnv i ((j, v) : env)
  | i == j    = v
  | otherwise = lookupEnv i env


numV :: ASAValues -> Int
numV (NumV n) = n
numV _ = error "Error de tipo: se esperaba NumV"

boolV :: ASAValues -> Bool
boolV (BooleanV b) = b
boolV _ = error "Error de tipo: se esperaba BooleanV"

pairV1 :: ASAValues -> ASAValues
pairV1 (PairV v1 _) = v1
pairV1 _ = error "Error de tipo: se esperaba PairV"

pairV2 :: ASAValues -> ASAValues
pairV2 (PairV _ v2) = v2
pairV2 _ = error "Error de tipo: se esperaba PairV"

closureP :: ASAValues -> String
closureP (ClosureV p _ _) = p

closureC :: ASAValues -> ASAValues
closureC (ClosureV _ c _) = c

closureE :: ASAValues -> Env
closureE (ClosureV _ _ e) = e