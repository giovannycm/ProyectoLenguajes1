module Main where

import Lex
import Desugar
import Grammars
import Interp

-- Ambiente con el combinador Z
combinadorZ :: String
combinadorZ =
  "(lambda (f) \
  \  ((lambda (x) \
  \    (f (lambda (v) ((x x) v)))) \
  \   (lambda (x) \
  \    (f (lambda (v) ((x x) v))))))"

z :: ASAValues
z =
  let sasa = parse (lexer combinadorZ)
      asa  = desugar sasa
  in interp (desugarV asa) []

prelude :: Env
prelude = [("Z", z)]

-- Pretty-printer
saca :: ASAValues -> String
saca (NumV n)         = show n
saca (BooleanV True)  = "#t"
saca (BooleanV False) = "#f"
saca (NilV)           = "[]"
saca (PairV v1 v2)    = "(" ++ saca v1 ++ " . " ++ saca v2 ++ ")"
saca (ClosureV _ _ _) = "#<procedure>"
saca (ExprV v _)      = "#<expr> " ++ saca v
saca v                = "#<valor-desconocido: " ++ show v ++ ">"

-- Función auxiliar para evaluar
eval :: String -> String
eval x = saca (interp (desugarV (desugar (parse (lexer x)))) prelude)

-- Función para ejecutar una prueba
test :: String -> String -> String -> IO ()
test nombre expr esperado = do
  putStr $ nombre ++ ": "
  let resultado = eval expr
  if resultado == esperado
    then putStrLn $ "[PASS] " ++ resultado
    else putStrLn $ "[FAIL] esperado: " ++ esperado ++ ", obtenido: " ++ resultado

main :: IO ()
main = do
  putStrLn "=== PRUEBAS MINILISP ==="
  putStrLn ""
  
  -- Operadores aritméticos básicos
  putStrLn "--- Aritmetica Basica ---"
  test "Suma 2 args" "(+ 3 5)" "8"
  test "Suma 3 args" "(+ 1 2 3)" "6"
  test "Suma 4 args" "(+ 10 20 30 40)" "100"
  test "Resta 2 args" "(- 10 3)" "7"
  test "Resta 3 args" "(- 100 20 5)" "75"
  test "Mult 2 args" "(* 4 5)" "20"
  test "Mult 3 args" "(* 2 3 4)" "24"
  test "Div 2 args" "(/ 20 4)" "5"
  test "Div 3 args" "(/ 100 5 2)" "10"
  putStrLn ""
  
  -- Operadores unarios
  putStrLn "--- Operadores Unarios ---"
  test "add1" "(add1 5)" "6"
  test "sub1" "(sub1 10)" "9"
  test "sqrt" "(sqrt 16)" "4"
  test "expt" "(expt 2 3)" "8"
  test "expt 2" "(expt 5 2)" "25"
  putStrLn ""
  
  -- Operadores relacionales
  putStrLn "--- Operadores Relacionales ---"
  test "= true" "(= 5 5)" "#t"
  test "= false" "(= 5 3)" "#f"
  test "= 3 args true" "(= 2 2 2)" "#t"
  test "= 3 args false" "(= 2 2 3)" "#f"
  test "!= true" "(!= 5 3)" "#t"
  test "!= false" "(!= 5 5)" "#f"
  test "< true" "(< 3 5)" "#t"
  test "< false" "(< 5 3)" "#f"
  test "<= true" "(<= 3 3)" "#t"
  test "<= 3 args" "(<= 1 2 3)" "#t"
  test "> true" "(> 5 3)" "#t"
  test ">= true" "(>= 5 5)" "#t"
  putStrLn ""
  
  -- Booleanos
  putStrLn "--- Booleanos ---"
  test "not true" "(not #f)" "#t"
  test "not false" "(not #t)" "#f"
  putStrLn ""
  
  -- Condicionales
  putStrLn "--- Condicionales ---"
  test "if0 true" "(if0 0 10 20)" "10"
  test "if0 false" "(if0 5 10 20)" "20"
  test "if true" "(if #t 100 200)" "100"
  test "if false" "(if #f 100 200)" "200"
  test "if con <" "(if (< 3 5) 1 0)" "1"
  putStrLn ""
  
  -- Let y Let*
  putStrLn "--- Let y Let* ---"
  test "let simple" "(let ((x 5)) x)" "5"
  test "let 2 vars" "(let ((x 10) (y 20)) (+ x y))" "30"
  test "let 3 vars" "(let ((a 1) (b 2) (c 3)) (+ a (+ b c)))" "6"
  test "let*" "(let* ((x 5) (y (+ x 3))) y)" "8"
  test "let* 3 vars" "(let* ((x 2) (y (* x 3)) (z (+ y 1))) z)" "7"
  putStrLn ""
  
  -- Pares
  putStrLn "--- Pares ---"
  test "pair" "(pair 1 2)" "(1 . 2)"
  test "fst" "(fst (pair 10 20))" "10"
  test "snd" "(snd (pair 10 20))" "20"
  test "pair anidado" "(pair 1 (pair 2 3))" "(1 . (2 . 3))"
  putStrLn ""
  
  -- Listas
  putStrLn "--- Listas ---"
  test "lista vacia" "[]" "[]"
  test "lista [1,2,3]" "[1, 2, 3]" "(1 . (2 . (3 . [])))"
  test "head" "(head [1, 2, 3])" "1"
  test "tail" "(tail [1, 2, 3])" "(2 . (3 . []))"
  test "head tail" "(head (tail [10, 20, 30]))" "20"
  putStrLn ""
  
  -- Lambdas
  putStrLn "--- Lambdas ---"
  test "lambda 1 arg" "((lambda (x) (+ x 1)) 5)" "6"
  test "lambda 2 args" "((lambda (x y) (+ x y)) 3 7)" "10"
  test "lambda 3 args" "((lambda (x y z) (+ x (+ y z))) 5 10 15)" "30"
  test "lambda anidado" "((lambda (x) ((lambda (y) (+ x y)) 3)) 5)" "8"
  putStrLn ""
  
  -- Letrec (recursión)
  putStrLn "--- Letrec (Recursion) ---"
  test "suma recursiva" "(letrec (sumN (lambda (n) (if (<= n 0) 0 (+ n (sumN (sub1 n)))))) (sumN 5))" "15"
  test "factorial" "(letrec (fact (lambda (n) (if (<= n 1) 1 (* n (fact (sub1 n)))))) (fact 5))" "120"
  test "fibonacci" "(letrec (fib (lambda (n) (if (<= n 1) 1 (+ (fib (sub1 n)) (fib (sub1 (sub1 n))))))) (fib 6))" "13"
  putStrLn ""
  
  -- Funciones de orden superior
  putStrLn "--- Funciones de Orden Superior ---"
  test "map" "(letrec (map (lambda (f l) (if (= l []) [] (pair (f (head l)) (map f (tail l)))))) (map (lambda (x) (* x 2)) [1, 2, 3]))" "(2 . (4 . (6 . [])))"
  test "filter" "(letrec (filter (lambda (p l) (if (= l []) [] (if (p (head l)) (pair (head l) (filter p (tail l))) (filter p (tail l)))))) (filter (lambda (x) (>= x 3)) [1, 5, 2, 4, 3]))" "(5 . (4 . (3 . [])))"
  putStrLn ""
  
  putStrLn "=== FIN DE PRUEBAS ==="