# MINILISP

Intérprete de un subconjunto extendido de LISP implementado en Haskell.

## Requisitos

* GHC (Glasgow Haskell Compiler)

* Alex (generador léxico)

* Happy (generador de parser)

## Compilación

Desde la raíz del proyecto:

```bash
cd src
alex Lex.x
happy Grammars.y
ghc --make MiniLisp.hs -o minilisp
```

## Ejecución

### REPL Interactivo

```bash
cd src
./minilisp
```

O desde la raíz:

```bash
./src/minilisp
```

### Ejecutar Tests

```bash
cd src
ghc --make Tests.hs -o tests
./tests
```

## Limpiar archivos generados

```bash
cd src
del *.hi *.o Grammars.hs Lex.hs *.exe
```

## Ejemplos de uso

```lisp
> (+ 1 2 3)
6

> (let ((x 5) (y 10)) (* x y))
50

> ((lambda (x y) (+ x y)) 3 4)
7

> [1, 2, 3, 4]
(1 . (2 . (3 . (4 . []))))

> (letrec (fact (lambda (n) (if0 n 1 (* n (fact (sub1 n)))))) (fact 5))
120
```

## Extensiones implementadas

* Operadores aritméticos variádicos: `+`, `-`, `*`, `/`

* Operadores unarios: `add1`, `sub1`, `sqrt`, `expt`

* Predicados relacionales variádicos: `=`, `!=`, `<`, `<=`, `>`, `>=`

* Operadores lógicos: `not`

* Condicionales: `if`, `if0`, `cond`

* Enlaces locales: `let`, `let*` (variádicos)

* Recursión: `letrec` (con Z combinator)

* Pares ordenados: `pair`, `fst`, `snd`

* Listas: sintaxis `[]`, operadores `head`, `tail`

* Lambdas variádicas con currificación

* Funciones de orden superior: `map`, `filter`

## Estructura del proyecto

```
.
├── README.md
└── src/
    ├── Lex.x           # Especificación léxica (Alex)
    ├── Grammars.y      # Gramática (Happy)
    ├── MiniLisp.hs     # REPL principal
    ├── Desugar.hs      # Eliminación de azúcar sintáctica
    ├── Interp.hs       # Intérprete (semántica operacional)
    └── Tests.hs        # Suite de pruebas
```