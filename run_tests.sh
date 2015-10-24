#!/bin/bash
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'
rm -f testes/*.myout
for i in **testes/*.lisp; do 
	if [ "$i" != "testes/teste06.lisp" ] ; then
		eval "clisp $i >> ${i::-4}myout"
	fi
done

for i in **testes/*.out; do
	if ! diff "$i" "${i::-3}myout" > /dev/null ; then
	  printf "${RED}Test $i failed ${NC}\n"
	else
		printf "${GREEN} ${i::-3}lisp passed ${NC}\n"
	fi
done
