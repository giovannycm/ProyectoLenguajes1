module Main where

import SistemaTipos
import InterpTipos


-- Ejemplo: (\x:Int. x + 1) 5
-- Lógicamente: (Int -> Int) aplicado a Int. 
-- Esperamos: Que reduzca a 6 y que SIEMPRE tenga tipo Int.
prog1 :: ASA
prog1 = App 
          (Fun "x" TInt (Add (Id "x") (Num 1))) 
          (Num 5)

-- Ejemplo Lógico: (\x:Bool. if x then 1 else 0) True
-- Lógicamente: (Bool -> Int) aplicado a Bool. Resultado esperado: Int
prog2 :: ASA
prog2 = App
          (Fun "x" TBool 
            (If (Id "x") (Num 1) (Num 0)))
          (Boolean True)

-- Función que ejecuta paso a paso y verifica tipos
runTrace :: ASA -> IO ()
runTrace term = do
    putStrLn $ "---------------------------------------------------"
    putStrLn $ "Termino: " ++ show term
    
    -- 1. Verificar Semántica Estática (Type Check)
    case typeof [] term of
        Left err -> do
            putStrLn $ "  [X] ERROR DE TIPOS: " ++ err
            putStrLn "  >>> La demostración se detiene por inconsistencia lógica."
            
        Right t -> do
            putStrLn $ "  [OK] Tipo verificado: " ++ show t
            
            -- 2. Verificar si es valor final (Forma Normal)
            if isValue term
                then putStrLn "  >>> Forma Normal alcanzada. Fin."
                else do
                    -- 3. Dar un paso de Semántica Dinámica
                    putStrLn "  --> Reduciendo (Paso Pequeño)..."
                    runTrace (smallStep term)

main :: IO ()
main = do
    putStrLn "=== PROYECTO 02 ==="
    putStrLn "Este programa demuestra que la reducción (ejecución) preserva el tipo (lógica)."
    
    putStrLn "\n[CASO 1] Función Aritmética Simple"
    runTrace prog1
    
    putStrLn "\n[CASO 2] Lógica Booleana"
    runTrace prog2