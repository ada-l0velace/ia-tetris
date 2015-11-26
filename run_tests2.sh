#!/bin/bash
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'
rm -f testes/*.myout

function run_tests {
	rm -f "testes/testes publicos/test$1/out"
	rm -f "testes/testes publicos/test$1/myout"
	rm -f "testes/testes publicos/test$1/myout.txt"
	rm -f "testes/testes publicos/test$1/temp"
	sed '/^;/ d' < "testes/testes publicos/test$1/output" > "testes/testes publicos/test$1/out"
	clisp -i -q -C proj.lisp < "testes/testes publicos/test$1/input" > "testes/testes publicos/test$d/myout"
	sed '/^;/ d' "testes/testes publicos/test$1/myout" > "testes/testes publicos/test$1/temp"
	sed -e ':a;N;$!ba;s/0\ errors,\ 0\ warnings\n//g' "testes/testes publicos/test$1/temp" > "testes/testes publicos/test$1/temp2"
	mv "testes/testes publicos/test$1/temp2" "testes/testes publicos/test$1/temp" 
	rm -f "testes/testes publicos/test$1/myout"
	mv "testes/testes publicos/test$1/temp" "testes/testes publicos/test$1/myout"
	rm -f "testes/testes publicos/test$1/temp"
	perl -pi -e 'chomp if eof' "testes/testes publicos/test$1/myout"
	perl -pi -e 'chomp if eof' "testes/testes publicos/test$1/out"
	if ! diff "testes/testes publicos/test$1/out" "testes/testes publicos/test$1/myout" > /dev/null ; then
		printf "${RED} Test $1 failed ${NC}\n"
	else
		printf "${GREEN} Test $1 passed ${NC}\n"
	fi
}

for d in {01..12} 14 16 18 19 {22..26} 30; do
	run_tests $d
done

#for i in **testes/*.lisp; do 
#	if [ "$i" != "testes/teste06.lisp" ] ; then
#		eval "clisp $i >> ${i::-4}myout"
#	fi
#done

#for i in **testes/*.out; do
#	if ! diff "$i" "${i::-3}myout" > /dev/null ; then
#	  printf "${RED}Test $i failed ${NC}\n"
#	else
#		printf "${GREEN} ${i::-3}lisp passed ${NC}\n"
#	fi
#done
