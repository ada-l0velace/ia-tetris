(load "proj.lisp")
(defvar e2 (make-estado :tabuleiro (cria-tabuleiro-aleatorio 0.0 0.3) :pecas-por-colocar '(i)))
(executa-jogadas e2 (accoes e2))