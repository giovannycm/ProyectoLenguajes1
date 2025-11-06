module Main where

import Lex
import Desugar
import Grammars
import Interp
import System.IO (hFlush, stdout)
import Control.Exception (catch, SomeException)

-- Combinador Z 
combinadorZ :: String
combinadorZ =
  "(lambda (f)    \
  \  ((lambda (x)    \
  \    (f (lambda (v) ((x x) v))))    \
  \   (lambda (x)    \
  \    (f (lambda (v) ((x x) v))))"

-- Valor de Z ya desazucarado y evaluado en []
z :: ASAValues
z =
  let sasa = parse (lexer combinadorZ)  -- SASA
      asa  = desugar sasa    -- ASA
  in interp (desugarV asa) []    -- ASAValues evaluado en []

-- Pretty-printer sencillo
saca :: ASAValues -> String
saca (NumV n)    = show n
saca (BooleanV True)    = "#t"
saca (BooleanV False)   = "#f"
saca (NilV)    = "[]"
saca (PairV v1 v2)    = "(" ++ saca v1 ++ " . " ++ saca v2 ++ ")"
saca (ClosureV _ _ _)   = "#<procedure>"
saca (ExprV v _)    = "#<expr> " ++ saca v 
saca v    = "#<valor-desconocido: " ++ show v ++ ">"

-- Ambiente inicial: ligamos Z a su valor
prelude :: Env
prelude = [("Z", z)]

repl :: IO ()
repl = do
    putStr "> "
    hFlush stdout 
    str <- getLine
    if str == "(exit)"
      then putStrLn "Bye."
      else do
        -- Manejo de errores simple
        let action = do
              let sasa = parse (lexer str)
              let asa  = desugar sasa
              let val  = interp (desugarV asa) prelude
              putStrLn (saca val)
        action `catch` (\e -> putStrLn (show (e :: SomeException)))
        repl

run :: IO ()
run = do
  putStrLn "Mini-Lisp (versión ansiosa). Bienvenidx."
  putStrLn "Escribe :q para salir."
  repl

main :: IO ()
main = run

test :: String -> IO ()
test x = putStrLn $ saca (interp (desugarV (desugar (parse (lexer x)))) prelude)

-- Pruebas
testSuma    = test "(letrec (sumN (lambda (n) (if (<= n 0) 0 (+ n (sumN (sub1 n)))) (sumN 3))"    -- 6
testFactorial  = test "(letrec (fact (lambda (n) (if (<= n 1) 1 (* n (fact (sub1 n)))) (fact 5))" -- 120
testFibo    = test "(letrec (fib (lambda (n) (if (<= n 1) 1 (+ (fib (sub1 n)) (fib (sub1 (sub1 n)))) (fib 6))" -- 8 (1 1 2 3 5 8)

-- Pruebas o (map y filter)
testMap = test "(letrec (map (lambda (f l) (if (eq l nil) nil (pair (f (head l)) (map f (tail l)))) (map (lambda (x) (* x 2)) '[1, 2, 3]))"

testFilter = test "(letrec (filter (lambda (f l) (if (eq l nil) nil (if (f (head l)) (pair (head l) (filter f (tail l))) (filter f (tail l)))) (filter (lambda (x) (<= 2 x)) '[0, 5, 1, 3]))"

-- Prueba para 'let' y 'lambda' variádicos
testLet = test "(let ((x 10) (y 20)) (+ x y))" 
testLambda = test "((lambda (x y z) (+ x (+ y z))) 5 10 15)" 