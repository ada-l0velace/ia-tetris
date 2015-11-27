(defparameter *dim-linhas* 18)
(defparameter *dim-colunas* 10)

(defun cria-tabuleiro ()
	(mapcar #'(lambda (x) (declare (ignore x))(make-array (list *dim-colunas*) :element-type '(mod 2) :initial-element 0)) (make-list *dim-linhas*)))

(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
	(aref (nth linha tabuleiro) (- 9 coluna)))

(defun tabuleiro-linha (tabuleiro linha)
	(nth linha tabuleiro))

(defun tabuleiro-buracos (tabuleiro)
	(let (
		(grid-mask (ash (make-array (list *dim-colunas*) :element-type '(mod 2) :initial-contents #*0000000001) *dim-colunas*))
		(under-mask 0)
		(l-neighbor-mask 0)
		(r-neighbor-mask 0)
		(found-holes 0)
		(line 0)
		(filled 0)
		(min-y 0)
		)

		(loop while 
			(and 
				(< min-y *dim-linhas*) 
				(eq (tabuleiro-linha tabuleiro min-y) grid-mask)
			) 
		do 
			(incf min-y)
		)
		;(format T "~d~c" grid-mask #\linefeed)
		(loop for y from min-y below *dim-linhas* do
			(setf line (tabuleiro-linha tabuleiro y))
			(setf filled (bit-and (bit-not line) grid-mask))
			;(format T "~d~c" under-mask #\linefeed)
			(setf under-mask (bit-ior under-mask filled))
			;(format T "~d~c" filled #\linefeed)
			;(format T "~d~c" under-mask #\linefeed)
			(setf l-neighbor-mask (bit-ior l-neighbor-mask (ash filled 1)))
			(setf r-neighbor-mask (bit-ior r-neighbor-mask (ash filled -1)))
			;(format T "~d~c" (integer-length (logand r-neighbor-mask line)) #\linefeed)
			(incf found-holes 
				(+
					(integer-length (logand under-mask line))
					(integer-length (logand l-neighbor-mask line))
					(integer-length (logand r-neighbor-mask line))
				)
			)
		)
	found-holes)
)

