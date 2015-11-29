#!/bin/bash
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'
rm -f testes/*.myout
rm -f testes/*.out

function run_tests {
	mytime="$( TIMEFORMAT='%lU'; time ( clisp run-test.fas < "testes/testes publicos/test$1/input" > "testes/testes publicos/test$d/myout" ) 2>&1 1>/dev/null )"	
	if ! diff "testes/testes publicos/test$1/output" "testes/testes publicos/test$1/myout" ; then
		printf "${RED} Test $1 failed $mytime ${NC}\n"
	else
		printf "${GREEN} Test $1 passed $mytime ${NC}\n"
	fi	
}
#'$s/$/\n/'
sed "1s/^/(defconstant peca-i0 (make-array (list 4 1) :initial-element T)) (defconstant peca-i1 (make-array (list 1 4) :initial-element T)) (defconstant peca-l0 (make-array (list 3 2) :initial-contents '((T T)(T nil)(T nil)))) (defconstant peca-l1 (make-array (list 2 3) :initial-contents '((T nil nil)(T T T)))) (defconstant peca-l2 (make-array (list 3 2) :initial-contents '((nil T)(nil T)(T T)))) (defconstant peca-l3 (make-array (list 2 3) :initial-contents '((T T T)(nil nil T)))) (defconstant peca-j0 (make-array (list 3 2) :initial-contents '((T T)(nil T)(nil T)))) (defconstant peca-j1 (make-array (list 2 3) :initial-contents '((T T T)(T nil nil)))) (defconstant peca-j2 (make-array (list 3 2) :initial-contents '((T nil)(T nil)(T T)))) (defconstant peca-j3 (make-array (list 2 3) :initial-contents '((nil nil T)(T T T)))) (defconstant peca-o0 (make-array (list 2 2) :initial-element T)) (defconstant peca-s0 (make-array (list 2 3) :initial-contents '((T T nil)(nil T T)))) (defconstant peca-s1 (make-array (list 3 2) :initial-contents '((nil T)(T T)(T nil)))) (defconstant peca-z0 (make-array (list 2 3) :initial-contents '((nil T T)(T T nil)))) (defconstant peca-z1 (make-array (list 3 2) :initial-contents '((T nil)(T T)(nil T)))) (defconstant peca-t0 (make-array (list 2 3) :initial-contents '((T T T)(nil T nil)))) (defconstant peca-t1 (make-array (list 3 2) :initial-contents '((T nil)(T T)(T nil)))) (defconstant peca-t2 (make-array (list 2 3) :initial-contents '((nil T nil)(T T T)))) (defconstant peca-t3 (make-array (list 3 2) :initial-contents '((nil T)(T T)(nil T))))\n/" proj.lisp > run-test.lisp
APPEND="(loop (let ((line (read-line *standard-input* NIL))) (when (not line) (return)) (if (equal #\; (char (string-trim \" \" line) 0)) (format T \"~A~%\" line) (let ((result (multiple-value-list (eval (read-from-string line))))) (format T \"~A\" (first result)) (dolist (other-value (rest result)) (format T \"; ~A\" other-value)) (format T \"~%\")))))"
FILE_NAME="$(cat run-test.lisp)"
echo "$FILE_NAME $APPEND" > run-test.lisp
clisp -q -C -c run-test.lisp > /dev/null
for d in {01..27}; do
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
