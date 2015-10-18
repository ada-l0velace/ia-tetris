(load "proj.lisp")
(defvar t0 (cria-tabuleiro))
(defvar p0 (formulacao-problema t0 '(o)))
(procura-pp p0)