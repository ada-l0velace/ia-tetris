(ignore-value (setf a1 '#2A((T T T T NIL NIL T T T T)(T T T NIL NIL NIL T T T T)(T T T NIL NIL NIL T T T T)(T T T NIL NIL NIL T T T T)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))))
(ignore-value (setf a2 (tabuleiro->array (cria-tabuleiro))))

(ignore-value (setf p1 '(i i i i i i i)))
(ignore-value (setf e1 (make-estado :tabuleiro (cria-tabuleiro) :pecas-por-colocar p1 :pontos 0 :pecas-colocadas '())))
;(desenha-estado e1)
; (time (setf r1 (procura-best (tabuleiro->array (cria-tabuleiro)) p1)))
; (executa-jogadas e1 r1 NIL)

(loop for lista in ('(s l t z z)
'(s l t z z)
'(s l z t z)
'(s l z z t)
'(s l z t z)
'(s l z z t)
'(s t l z z)
'(s t l z z)
'(s t z z l)
'(s t z l z)
'(s t z z l)
'(s t z l z)
'(s z l t z)
'(s z l z t)
'(s z t l z)
'(s z t z l)
'(s z z l t)
'(s z z t l)
'(s z l z t)
'(s z l t z)
'(s z t z l)
'(s z t l z)
'(s z z t l)
'(s z z l t)
'(l s z t z)
'(l s z z t)
'(l s t z z)
'(l s t z z)
'(l s z z t)
'(l s z t z)
'(l t z z s)
'(l t z s z)
'(l t s z z)
'(l t s z z)
'(l t z s z)
'(l t z z s)
'(l z z s t)
'(l z z t s)
'(l z s z t)
'(l z s t z)
'(l z t z s)
'(l z t s z)
'(l z z t s)
'(l z z s t)
'(l z s t z)
'(l z s z t)
'(l z t s z)
'(l z t z s)
'(t s z z l)
'(t s z l z)
'(t s z z l)
'(t s z l z)
'(t s l z z)
'(t s l z z)
'(t l z s z)
'(t l z z s)
'(t l z s z)
'(t l z z s)
'(t l s z z)
'(t l s z z)
'(t z l z s)
'(t z l s z)
'(t z z l s)
'(t z z s l)
'(t z s l z)
'(t z s z l)
'(t z l s z)
'(t z l z s)
'(t z z s l)
'(t z z l s)
'(t z s z l)
'(t z s l z)
'(z s l t z)
'(z s l z t)
'(z s t l z)
'(z s t z l)
'(z s z l t)
'(z s z t l)
'(z l s z t)
'(z l s t z)
'(z l t z s)
'(z l t s z)
'(z l z t s)
'(z l z s t)
'(z t s l z)
'(z t s z l)
'(z t l s z)
'(z t l z s)
'(z t z s l)
'(z t z l s)
'(z z s t l)
'(z z s l t)
'(z z l t s)
'(z z l s t)
'(z z t l s)
'(z z t s l)
'(z s z l t)
'(z s z t l)
'(z s l z t)
'(z s l t z)
'(z s t z l)
'(z s t l z)
'(z l z t s)
'(z l z s t)
'(z l s t z)
'(z l s z t)
'(z l t s z)
'(z l t z s)
'(z t z s l)
'(z t z l s)
'(z t s z l)
'(z t s l z)
'(z t l z s)
'(z t l s z)
'(z z t l s)
'(z z t s l)
'(z z s l t)
'(z z s t l)
'(z z l s t)
'(z z l t s)) do
(time (setf r1 (procura-best (tabuleiro->array (cria-tabuleiro)) p1)))
(executa-jogadas e1 r1 NIL)
)