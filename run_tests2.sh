#!/bin/bash
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'
rm -f testes/*.myout

for d in {1..12}; do
	if [ $d -lt 10 ] ; then
		rm -f "testes/testes publicos/test0$d/out"
		rm -f "testes/testes publicos/test0$d/myout"
		rm -f "testes/testes publicos/test0$d/myout.txt"
		rm -f "testes/testes publicos/test0$d/temp"
		sed '/^;/ d' < "testes/testes publicos/test0$d/output" > "testes/testes publicos/test0$d/out"
		clisp -i proj.lisp < "testes/testes publicos/test0$d/input" > "testes/testes publicos/test0$d/myout"
		tail -n+26 "testes/testes publicos/test0$d/myout" > "testes/testes publicos/test0$d/temp"
		head -n -1 "testes/testes publicos/test0$d/temp" > "testes/testes publicos/test0$d/temp2"
		rm -f "testes/testes publicos/test0$d/myout"
		mv "testes/testes publicos/test0$d/temp2" "testes/testes publicos/test0$d/myout"
		rm -f "testes/testes publicos/test0$d/temp"
		rm -f "testes/testes publicos/test0$d/temp2" 
		perl -pi -e 'chomp if eof' "testes/testes publicos/test0$d/myout"
		perl -pi -e 'chomp if eof' "testes/testes publicos/test0$d/out"
		if ! diff "testes/testes publicos/test0$d/out" "testes/testes publicos/test0$d/myout" > /dev/null ; then
			printf "${RED} Test $d failed ${NC}\n"
		else
			printf "${GREEN} Test $d passed ${NC}\n"
		fi
	else
		rm -f "testes/testes publicos/test$d/out"
		rm -f "testes/testes publicos/test$d/myout"
		rm -f "testes/testes publicos/test$d/myout.txt"
		rm -f "testes/testes publicos/test$d/temp"
		sed '/^;/ d' < "testes/testes publicos/test$d/output" > "testes/testes publicos/test$d/out"
		clisp -i proj.lisp < "testes/testes publicos/test$d/input" > "testes/testes publicos/test$d/myout"
		tail -n+26 "testes/testes publicos/test$d/myout" > "testes/testes publicos/test$d/temp"
		head -n -1 "testes/testes publicos/test$d/temp" > "testes/testes publicos/test$d/temp2"
		rm -f "testes/testes publicos/test$d/myout"
		mv "testes/testes publicos/test$d/temp2" "testes/testes publicos/test$d/myout"
		rm -f "testes/testes publicos/test$d/temp"
		rm -f "testes/testes publicos/test$d/temp2" 
		perl -pi -e 'chomp if eof' "testes/testes publicos/test$d/myout"
		perl -pi -e 'chomp if eof' "testes/testes publicos/test$d/out"
		if ! diff "testes/testes publicos/test$d/out" "testes/testes publicos/test$d/myout" > /dev/null ; then
			printf "${RED} Test $d failed ${NC}\n"
		else
			printf "${GREEN} Test $d passed ${NC}\n"
		fi
	fi

done

for d in 14 16 18 19 22 23 24 26; do
	rm -f "testes/testes publicos/test$d/out"
	rm -f "testes/testes publicos/test$d/myout"
	rm -f "testes/testes publicos/test$d/myout.txt"
	rm -f "testes/testes publicos/test$d/temp"	
	sed '/^;/ d' < "testes/testes publicos/test$d/output" > "testes/testes publicos/test$d/out"
	clisp -i proj.lisp < "testes/testes publicos/test$d/input" > "testes/testes publicos/test$d/myout"
	tail -n+26 "testes/testes publicos/test$d/myout" > "testes/testes publicos/test$d/temp"
	head -n -1 "testes/testes publicos/test$d/temp" > "testes/testes publicos/test$d/temp2"
	rm -f "testes/testes publicos/test$d/myout"
	mv "testes/testes publicos/test$d/temp2" "testes/testes publicos/test$d/myout"
	rm -f "testes/testes publicos/test$d/temp"
	rm -f "testes/testes publicos/test$d/temp2"
	perl -pi -e 'chomp if eof' "testes/testes publicos/test$d/myout"
	perl -pi -e 'chomp if eof' "testes/testes publicos/test$d/out"
	if ! diff "testes/testes publicos/test$d/out" "testes/testes publicos/test$d/myout" > /dev/null ; then
		printf "${RED} Test $d failed ${NC}\n"
	else
		printf "${GREEN} Test $d passed ${NC}\n"
	fi
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
