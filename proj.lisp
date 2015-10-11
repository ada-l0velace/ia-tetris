 (defparameter *dim-linhas* 18)
 (defparameter *dim-colunas* 10)
;; Tipo accao

(defstruct
	(accao
		(:constructor cria-accao (coluna peca))
	)
coluna peca)

;; Tipo tabuleiro

(defstruct
	(tabuleiro (:conc-name tr-))
tab)

;; Tipo Problema
(defstruct problema
	estado-inicial
	solucao
	accoes
	resultado
	custo-caminho
)

;;Tipo Tabuleiro
(defun cria-tabuleiro ()
	(make-tabuleiro :tab (make-array (list *dim-linhas* *dim-colunas*)))
)

;des
(defun copia-tabuleiro (tabuleiro)
	(copy-seq tabuleiro)
)

;des
(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
	(aref (tr-tab tabuleiro) linha coluna)
)

;(incf num)
;(decf num)
(defun tabuleiro-altura-coluna (tabuleiro coluna)
	(let (
		(altura 0)
		(dim-linhas (array-dimension (tr-tab tabuleiro) 0))
	)
	(loop for i from 0 below dim-linhas do
		(if (eq (tabuleiro-preenchido-p tabuleiro i coluna) T)
			(incf altura)
		)
	)
	altura)
)

(defun tabuleiro-linha-completa-p (tabuleiro linha)
	(let (
		(dim-colunas (array-dimension (tr-tab tabuleiro) 1))
	)
	(loop for i from 0 below dim-colunas do
		(if (eq (tabuleiro-preenchido-p tabuleiro linha i) NIL)
			(return-from tabuleiro-linha-completa-p NIL)
		)
	)
	T)
)

(defun tabuleiro-preeche! (tabuleiro linha coluna)
	(if (not (or (> linha *dim-linhas*) (> coluna *dim-colunas*)))
		(setf (aref (tr-tab tabuleiro) linha coluna) T)
	)
)

(defun tabuleiro-remove-linha! (tabuleiro linha)
	(loop for i from 0 below *dim-colunas* do
		(setf (aref (tr-tab tabuleiro) linha i) NIL)
	)
)

(defun tabuleiro-topo-preenchido-p (tabuleiro)
	(loop for i from 0 below *dim-colunas* do
		(if (eq (tabuleiro-preenchido-p tabuleiro (1- *dim-linhas*) i) T)
			(return-from tabuleiro-topo-preenchido-p T)
		)
	)
	NIL	
)

(defun tabuleiros-iguais-p (tabuleiro tabuleiro1)
	(loop for i from 0 below *dim-linhas* do
		(loop for j from 0 below *dim-colunas* do
			(if (not (eq (tabuleiro-preenchido-p tabuleiro i j) (tabuleiro-preenchido-p tabuleiro1 i j)))
				(return-from tabuleiros-iguais-p NIL)
			)
		)
	)
	T	
)

