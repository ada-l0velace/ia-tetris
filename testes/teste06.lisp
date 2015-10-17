(load "proj.lisp")
;(executa-jogadas (problema-estado-inicial p1) (procura-pp p1))

;(desenha-estado (procura-pp p1))

;(princ (pA*a p1 #' qualidade))
;(princ (procura-best t1 '(i o j l t)))
(executa-jogadas (make-estado :tabuleiro t1 :pecas-por-colocar '(i o j l t)) (procura-best t1 '(i o j l t)))