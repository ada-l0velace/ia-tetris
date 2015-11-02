(setf estado1 (make-estado :pontos 0 :pecas-por-colocar '(o o o o o j) :pecas-colocadas '() :tabuleiro (cria-tabuleiro)))
(desenha-estado estado1)
(setf estado2 (resultado estado1 '(0 . #2A((T T)(T T)))))
(desenha-estado estado2)
(setf estado2 (resultado estado2 '(2 . #2A((T T)(T T)))))
(desenha-estado estado2)
(setf estado2 (resultado estado2 '(4 . #2A((T T)(T T)))))
(desenha-estado estado2)
(setf estado2 (resultado estado2 '(6 . #2A((T T)(T T)))))
;;deve retornar T
(tabuleiro-preenchido-p (estado-tabuleiro estado2) 1 5)
;;deve retornar IGNORE
(desenha-estado estado2)
(setf estado2 (resultado estado2 '(8 . #2A((T T)(T T)))))
;;deve retornar 300
(estado-pontos estado2)
;;deve retornar NIL
(tabuleiro-preenchido-p (estado-tabuleiro estado2) 1 5)