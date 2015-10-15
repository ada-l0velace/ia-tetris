#!/bin/bash
rm -f testes/*.out
clisp testes/teste00.lisp >> testes/teste00.out
clisp testes/teste01.lisp >> testes/teste01.out
clisp testes/teste02.lisp >> testes/teste02.out
clisp testes/teste03.lisp >> testes/teste03.out
clisp testes/teste04.lisp >> testes/teste04.out
