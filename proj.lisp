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

;Tipo Tabuleiro
(defun cria-tabuleiro ()
	(make-tabuleiro :tab (make-array (list 18 10)))
)

(defun copia-tabuleiro (tabuleiro)
	(copy-seq tabuleiro)
)

(defun tabuleiro-preenchido-p (tabuleiro linha coluna):
	(aref (tr-tab tabuleiro) linha coluna)
)

(defun tabuleiro-altura-coluna (tabuleiro coluna):
	
)