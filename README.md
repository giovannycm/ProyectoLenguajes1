# MINILISP

Intérprete de un subconjunto extendido de LISP implementado en Haskell. Con modificaciones para el proyecto 2. 

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
ghc --make DemoProyecto.hs -o proyecto02
./proyecto02   

```

#