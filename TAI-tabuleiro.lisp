;;funcoes auxiliares
(defun listas-equivalentes (l1 l2)
   (cond ((null l1) (null l2))
   	 ((not (= (length l1) (length l2))) NIL)
   	 (T (listas-equivalentes (rest l1) (remove (first l1) l2 :test #'equal)))))

(defun all-list-p (f list)
   (if (null list)
       T
       (and (funcall f (first list))
       	    (all-list-p f (rest list)))))

;;tipo tabuleiro		
(defun cria-tabuleiro ()
	(mapcar #'(lambda (x) (declare (ignore x))(make-array (list 10))) (make-list 18)))
	
(defun tabuleiro-linha-completa-p (tabuleiro linha)
	(every #'identity (nth linha tabuleiro)))
	
(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
	(aref (nth linha tabuleiro) (- 9 coluna)))
	
(defun tabuleiro-preenche! (tabuleiro linha coluna)
	(when (and (<= 0 linha 17) (<= 0 coluna 9)) 
		(setf (aref (nth linha tabuleiro) (- 9 coluna)) T)))
	
(defun tabuleiro-remove-linha! (tabuleiro linha)
	(avanca-linha-recursivo! (nthcdr linha tabuleiro)))
	
(defun tabuleiro-altura-coluna (tabuleiro coluna)
	(let ((altura 0)
		  (numero-linha 0))
		(dolist (linha tabuleiro altura)
			(when (aref linha (- 9 coluna)) (setf altura (+ numero-linha 1)))
			(incf numero-linha))))
	
(defun avanca-linha-recursivo! (lista)
	(cond ((null lista) nil)
		  ((null (cdr lista)) (setf (car lista) (make-array (list 10))))
		  (T (setf (car lista) (car (cdr lista))) (avanca-linha-recursivo! (cdr lista))))) 
	
(defun tabuleiro-topo-preenchido-p (tabuleiro)
	(some #'identity (nth 17 tabuleiro)))
	
(defun tabuleiros-iguais-p (tabuleiro1 tabuleiro2)
	(dotimes (linha 18)
		(dotimes (coluna 10)
			(when (not (eq (tabuleiro-preenchido-p tabuleiro1 linha coluna) (tabuleiro-preenchido-p tabuleiro2 linha coluna)))
				(return-from tabuleiros-iguais-p nil))))
	T)
	
(defun copia-tabuleiro (tabuleiro)
	(mapcar #'(lambda (linha) (copy-seq linha)) tabuleiro))

(defun array->tabuleiro (a)
	(let ((tabuleiro (cria-tabuleiro)))
		(dotimes (linha (array-dimension a 0))
			(dotimes (coluna (array-dimension a 1))
				(when (aref a linha coluna)
					(tabuleiro-preenche! tabuleiro linha coluna))))
		tabuleiro))
		
(defun tabuleiro->array (tabuleiro)
	(let ((output (make-array (list 18 10))))
		(dotimes (linha 18)
			(dotimes (coluna 10)
				(when (tabuleiro-preenchido-p tabuleiro linha coluna)
					(setf (aref output linha coluna) T))))
		output))				
