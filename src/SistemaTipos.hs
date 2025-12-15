module SistemaTipos where

-- DEFINICIÓN DE TIPOS (Proposiciones)
data Type
    = TInt              -- "Es un entero"
    | TBool             -- "Es un booleano"
    | TArrow Type Type  -- "Implica" (A -> B)
    deriving (Eq)

instance Show Type where
    show TInt = "Int"
    show TBool = "Bool"
    show (TArrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"


--  SINTAXIS ABSTRACTA TIPIFICADA (AST)
-- Adaptamos los ASA originales para que las funciones tengan tipo explícito.
data ASA
  = Id String
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | If ASA ASA ASA
  | Fun String Type ASA  
  | App ASA ASA
  deriving (Show, Eq)

-- Contexto de Tipos (Gamma): Variables y sus tipos
type Context = [(String, Type)]


--ERIFICADOR DE TIPOS (Semántica Estática)

-- Implementa el juicio: Gamma |- e : T

lookupType :: String -> Context -> Either String Type
lookupType v [] = Left $ "Variable no declarada: " ++ v
lookupType v ((i,t):xs)
    | v == i    = Right t
    | otherwise = lookupType v xs

typeof :: Context -> ASA -> Either String Type
-- Axiomas
typeof _ (Num _)     = Right TInt
typeof _ (Boolean _) = Right TBool

-- Reglas para Operadores
typeof ctx (Add e1 e2) =
    case (typeof ctx e1, typeof ctx e2) of
        (Right TInt, Right TInt) -> Right TInt
        (Left e, _) -> Left e
        (_, Left e) -> Left e
        _ -> Left "Error de Tipos: Suma espera (Int, Int)"

typeof ctx (Sub e1 e2) =
    case (typeof ctx e1, typeof ctx e2) of
        (Right TInt, Right TInt) -> Right TInt
        _ -> Left "Error de Tipos: Resta espera (Int, Int)"

typeof ctx (Not e) =
    case typeof ctx e of
        (Right TBool) -> Right TBool
        _ -> Left "Error de Tipos: Not espera (Bool)"

typeof ctx (If c t e) =
    case (typeof ctx c, typeof ctx t, typeof ctx e) of
        (Right TBool, Right t1, Right t2) ->
            if t1 == t2 then Right t1 else Left "Error: Ramas del If con tipos distintos"
        (Right _, _, _) -> Left "Error: La condición del If debe ser Bool"
        (Left err, _, _) -> Left err

-- REGLAS DE LA LÓGICA (CURRY-HOWARD)

-- Regla de la Abstracción 
-- Para verificar \x:T. M, asumimos x:T y verificamos M
typeof ctx (Fun param tParam body) =
    let newCtx = (param, tParam) : ctx
    in case typeof newCtx body of
        Right tBody -> Right (TArrow tParam tBody) -- Resultado: TParam -> TBody
        Left err    -> Left err

-- Regla de la Aplicación 
-- Si f: A->B y arg: A, entonces (f arg): B
typeof ctx (App f arg) =
    case (typeof ctx f, typeof ctx arg) of
        (Right (TArrow t1 t2), Right tArg) ->
            if t1 == tArg
            then Right t2  
            else Left $ "Error de Tipos: Se esperaba argumento " ++ show t1 ++ " pero llegó " ++ show tArg
        (Right t, _) -> Left $ "Error: Se intenta aplicar " ++ show t ++ " como si fuera función."
        (Left e, _) -> Left e
        (_, Left e) -> Left e

typeof ctx (Id x) = lookupType x ctx