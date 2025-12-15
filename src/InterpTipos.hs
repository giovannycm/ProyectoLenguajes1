module InterpTipos where

import SistemaTipos

isValue :: ASA -> Bool
isValue (Num n) = True
isValue (Boolean b) = True
isValue (Fun _ _ _) = True -- Las funciones son valores
isValue _ = False

-- Sustitución: e[i := v]
sust :: ASA -> String -> ASA -> ASA
sust (Num n) i v = Num n
sust (Boolean b) i v = Boolean b
sust (Id x) i v = if x == i then v else (Id x)
sust (Add e1 e2) i v = Add (sust e1 i v) (sust e2 i v)
sust (Sub e1 e2) i v = Sub (sust e1 i v) (sust e2 i v)
sust (Not e) i v = Not (sust e i v)
sust (If c t e) i v = If (sust c i v) (sust t i v) (sust e i v)
-- OJO AQUÍ: La sustitución en Fun debe respetar el scope
sust (Fun p t c) i v = 
    if p == i 
    then Fun p t c -- Shadowing: no sustituimos dentro
    else Fun p t (sust c i v)
sust (App f a) i v = App (sust f i v) (sust a i v)

-- Un paso de reducción (Small Step)
smallStep :: ASA -> ASA
smallStep (Id i) = error $ "Variable libre en ejecución: " ++ i
smallStep (Num n) = Num n
smallStep (Boolean b) = Boolean b

-- Reducción de Operadores
smallStep (Add (Num n) (Num m)) = Num (n + m)
smallStep (Add (Num n) e2) = Add (Num n) (smallStep e2)
smallStep (Add e1 e2) = Add (smallStep e1) e2

smallStep (Sub (Num n) (Num m)) = Num (n - m)
smallStep (Sub (Num n) e2) = Sub (Num n) (smallStep e2)
smallStep (Sub e1 e2) = Sub (smallStep e1) e2

smallStep (Not (Boolean b)) = Boolean (not b)
smallStep (Not e) = Not (smallStep e)

smallStep (If (Boolean True) t e) = t
smallStep (If (Boolean False) t e) = e
smallStep (If c t e) = If (smallStep c) t e

-- === REDUCCIÓN BETA ===
-- (Fun p t c) es un valor, no se reduce por sí mismo.
smallStep (Fun p t c) = Fun p t c

-- Aplicación:
-- 1. Si el argumento es un valor, sustituimos (Beta-Reducción)
smallStep (App (Fun p t c) a)
    | isValue a = sust c p a
    | otherwise = App (Fun p t c) (smallStep a) -- Reducir argumento (Call-by-value)
-- 2. Si la función no es un valor (es otra App o un Redex), reducimos la función
smallStep (App f a) = App (smallStep f) a