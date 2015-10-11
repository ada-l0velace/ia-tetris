;; Tipo accao

(defstruct
	(accao
		(:constructor cria-accao (coluna peca))
	)
coluna peca)

;; Tipo tabuleiro

(defstruct
	(tabuleiro)
tab)


(defun cria-tabuleiro ()
	(make-tabuleiro :tab (make-array (list 18 10)))
)

