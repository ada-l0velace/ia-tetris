(load "proj.lisp")
(defvar t0 (cria-tabuleiro))
(defvar p0 (formulacao-problema t0 '(l)))
(procura-pp p0)