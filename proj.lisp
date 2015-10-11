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
	(make-tabuleiro :tab (make-array (list 18 10)))
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

