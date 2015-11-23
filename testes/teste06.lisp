<<<<<<< HEAD
(load "proj.lisp")
;;exemplo muito simples de um tabuleiro com a primeira e segunda linha quase todas preenchidas

=======
;(load "proj.lisp")
>>>>>>> 55 tests passing we need to speak with teacher about some tests
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
; (time (executa-jogadas (problema-estado-inicial p1) (procura-pp p1)))
(format t "---------------------~c" #\linefeed)
(format t "---------------------~c" #\linefeed)
(format t "------Procura-A*-----~c" #\linefeed)
(format t "---------------------~c" #\linefeed)
(format t "---------------------~c" #\linefeed)
(time (executa-jogadas (problema-estado-inicial p1) (procura-A* p1 #'(lambda (x) 0))))
(format t "---------------------~c" #\linefeed)
(format t "---------------------~c" #\linefeed)
(format t "-----Procura-best----~c" #\linefeed)
(format t "---------------------~c" #\linefeed)
(format t "---------------------~c" #\linefeed)
<<<<<<< HEAD
;(time (executa-jogadas (problema-estado-inicial p1) (procura-best (estado-tabuleiro (problema-estado-inicial p1)) (estado-pecas-por-colocar (problema-estado-inicial p1)))))
=======
(time (executa-jogadas (problema-estado-inicial p1) (procura-best (tr-tab (estado-tabuleiro (problema-estado-inicial p1))) (estado-pecas-por-colocar (problema-estado-inicial p1)))))
>>>>>>> 55 tests passing we need to speak with teacher about some tests

 ;(procura-best (estado-tabuleiro (problema-estado-inicial p1)) (estado-pecas-por-colocar (problema-estado-inicial p1)))
;(procura-best-b (estado-tabuleiro (problema-estado-inicial p1)) (estado-pecas-por-colocar (problema-estado-inicial p1)))
;(recur_test 0)
;(desenha-estado (procura-best-b (estado-tabuleiro (problema-estado-inicial p1)) (estado-pecas-por-colocar (problema-estado-inicial p1))))
;(desenha-estado (procura-pp p1))
;(princ e1)
;(princ (accoes e1))

(procura-A* (make-problema :estado-inicial (make-estado :pontos 0 :tabuleiro t1 :pecas-colocadas () :pecas-por-colocar '(l j)) :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'qualidade) #'(lambda (x) 0))