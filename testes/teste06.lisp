(load "proj.lisp")
;(executa-jogadas (problema-estado-inicial p1) (procura-pp p1))

;(desenha-estado (procura-pp p1))

;(princ (pA*a p1 #' qualidade))
;(princ (procura-best t1 '(i o j l t)))
; (setf rdp (random-pecas 6))
(format t "---------------------~c" #\linefeed)
(format t "---------------------~c" #\linefeed)
(format t "------Procura-pp-----~c" #\linefeed)
(format t "---------------------~c" #\linefeed)
(format t "---------------------~c" #\linefeed)
;(time (executa-jogadas (problema-estado-inicial p1) (procura-pp p1)))
(format t "---------------------~c" #\linefeed)
(format t "---------------------~c" #\linefeed)
(format t "------Procura-A*-----~c" #\linefeed)
(format t "---------------------~c" #\linefeed)
(format t "---------------------~c" #\linefeed)
(time (executa-jogadas (problema-estado-inicial p1) (procura-A* p1 #' heuristicas)))
(format t "---------------------~c" #\linefeed)
(format t "---------------------~c" #\linefeed)
(format t "-----Procura-best----~c" #\linefeed)
(format t "---------------------~c" #\linefeed)
(format t "---------------------~c" #\linefeed)
(time (executa-jogadas (problema-estado-inicial p1) (procura-best (estado-tabuleiro (problema-estado-inicial p1)) (estado-pecas-por-colocar (problema-estado-inicial p1)))))

 ;(procura-best (estado-tabuleiro (problema-estado-inicial p1)) (estado-pecas-por-colocar (problema-estado-inicial p1)))
;(procura-best-b (estado-tabuleiro (problema-estado-inicial p1)) (estado-pecas-por-colocar (problema-estado-inicial p1)))
;(recur_test 0)
;(desenha-estado (procura-best-b (estado-tabuleiro (problema-estado-inicial p1)) (estado-pecas-por-colocar (problema-estado-inicial p1))))
;(desenha-estado (procura-pp p1))
;(princ e1)
;(princ (accoes e1))