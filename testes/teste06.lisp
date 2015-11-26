(ignore-value (setf a1 '#2A((T T T T NIL NIL T T T T)(T T T NIL NIL NIL T T T T)(T T T NIL NIL NIL T T T T)(T T T NIL NIL NIL T T T T)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))))
(ignore-value (setf a2 (tabuleiro->array (cria-tabuleiro))))
;(ignore-value (setf p1 '(t i l l)))
(ignore-value (setf p1 '(t i l l)))
(ignore-value (setf e1 (make-estado :tabuleiro (array->tabuleiro a1) :pecas-por-colocar p1 :pontos 0 :pecas-colocadas '())))
(time (setf r1 (procura-best (tr-tab (estado-tabuleiro e1)) p1)))
(executa-jogadas e1 r1)













































