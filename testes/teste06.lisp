(load "proj.lisp")
;(executa-jogadas (problema-estado-inicial p1) (procura-pp p1))

;(desenha-estado (procura-pp p1))

;(princ (pA*a p1 #' qualidade))
;(princ (procura-best t1 '(i o j l t)))
;(setf rdp (random-pecas 5))
(executa-jogadas 
	(make-estado :tabuleiro (cria-tabuleiro-aleatorio 0 0) :pecas-por-colocar '(i o j l t i i i i)) 
 	(procura-best t1 '(i o j l t i i i i))
)
;(princ e1)
;(princ (accoes e1))