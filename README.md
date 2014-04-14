Compilation
===========

It needs:
- ocaml >= 4
- menhir (opam install menhir)
- batteries (opam install batteries)

Usage:
- make

Passes
======
1. Lexer (lexer.mll)
2. Parser (parse.mly)
3. Check types (semant.ml)
4. Translate to intermediate code (semant.ml, translate.ml)
5. Split code into basic blocks (bblock.ml)
6. Liveness analysis (TODO)
7. Register allocator (TODO)
8. Translate to assembler (z80machine.ml)

Related
=======
Modern Compiler in ML [https://github.com/rohankshir/compiler-sml]

TODO
====
* ir phi
* strings
* assign stmt
* && ||
* structs
