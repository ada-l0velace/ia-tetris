(load "proj.lisp")
(defvar pecas '(i l j o s z t))
(defvar t0 (cria-tabuleiro))
(defvar e0 NIL)
(loop for peca in pecas do
	(setf e0 (make-estado :tabuleiro t0 :pecas-por-colocar (cons peca NIL)))
	(setf accoes (accoes e0))
	(loop for accao in accoes do
		(executa-jogadas e0 (cons accao NIL))
	)
)
