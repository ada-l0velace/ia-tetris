
(defparameter *dim-linhas* 18)
(defparameter *dim-colunas* 10)
(defparameter *SUPPRESS-SIMILAR-CONSTANT-REDEFINITION-WARNING* T)
 
;; Tipo accao

(defstruct
	(accao)
coluna-peca)

;; Tipo tabuleiro

(defstruct
	(tabuleiro (:conc-name tr-))
tab)

;; Tipo Estado
(defstruct (estado 
	)
	(pontos 0 :type integer)
	pecas-por-colocar
	pecas-colocadas
	tabuleiro
)

;; Tipo Problema
(defstruct problema
	estado-inicial
	solucao
	accoes
	resultado
	custo-caminho
)

;;; definicao das configuracoes possiveis para cada peca
;;peca i 
(defconstant peca-i0 (make-array (list 4 1) :initial-element T))
(defconstant peca-i1 (make-array (list 1 4) :initial-element T))
;;peca l
(defconstant peca-l0 (make-array (list 3 2) :initial-contents '((T T)(T nil)(T nil))))
(defconstant peca-l1 (make-array (list 2 3) :initial-contents '((T nil nil)(T T T))))
(defconstant peca-l2 (make-array (list 3 2) :initial-contents '((nil T)(nil T)(T T))))
(defconstant peca-l3 (make-array (list 2 3) :initial-contents '((T T T)(nil nil T))))
;;peca j
(defconstant peca-j0 (make-array (list 3 2) :initial-contents '((T T)(nil T)(nil T))))
(defconstant peca-j1 (make-array (list 2 3) :initial-contents '((T T T)(T nil nil))))
(defconstant peca-j2 (make-array (list 3 2) :initial-contents '((T nil)(T nil)(T T))))
(defconstant peca-j3 (make-array (list 2 3) :initial-contents '((nil nil T)(T T T))))
;;peca o
(defconstant peca-o0 (make-array (list 2 2) :initial-element T))
;;peca s
(defconstant peca-s0 (make-array (list 2 3) :initial-contents '((T T nil)(nil T T))))
(defconstant peca-s1 (make-array (list 3 2) :initial-contents '((nil T)(T T)(T nil))))
;;peca z
(defconstant peca-z0 (make-array (list 2 3) :initial-contents '((nil T T)(T T nil))))
(defconstant peca-z1 (make-array (list 3 2) :initial-contents '((T nil)(T T)(nil T))))
;;peca t
(defconstant peca-t0 (make-array (list 2 3) :initial-contents '((T T T)(nil T nil))))
(defconstant peca-t1 (make-array (list 3 2) :initial-contents '((T nil)(T T)(T nil))))
(defconstant peca-t2 (make-array (list 2 3) :initial-contents '((nil T nil)(T T T))))
(defconstant peca-t3 (make-array (list 3 2) :initial-contents '((nil T)(T T)(nil T))))

;;;;;;;;;;;;;;;;;;;;;;
;;    Tipo peça     ;;
;;;;;;;;;;;;;;;;;;;;;;
(defun pecas(simbolo)
	(cond 
		((eq 'i simbolo) (cons peca-i0 (cons peca-i1 NIL)))
		((eq 'l simbolo) (cons peca-l0 (cons peca-l1 (cons peca-l2 (cons peca-l3 NIL)))))
		((eq 'j simbolo) (cons peca-j0 (cons peca-j1 (cons peca-j2 (cons peca-j3 NIL)))))
		((eq 'o simbolo) (cons peca-o0 NIL))
		((eq 's simbolo) (cons peca-s0 (cons peca-s1 NIL)))
		((eq 'z simbolo) (cons peca-z0 (cons peca-z1 NIL)))
		((eq 't simbolo) (cons peca-t0 (cons peca-t1 (cons peca-t2 (cons peca-t3 NIL)))))
	)
)

(defun peca-dimensao-largura (peca)
	(array-dimension peca 1)
)
(defun peca-dimensao-altura (peca)
	(array-dimension peca 0)
)

(defun peca-preenchido (peca linha coluna)
	(aref peca linha coluna)
)

(defun peca-pontos-maximo (peca)
	(case linhas-removidas 
		(('i) 800)
		(('j) 500)
		(('l) 500)
		(('s) 300)
		(('z) 300)
		(('t) 300)
		(('o) 300)
	)
)

(defun pecas-pontos-maximo (pecas)
	(let (
		(total 0)
		)
		(loop for peca in pecas do
			(setf total (+ total peca-pontos-maximos peca))
		)
	total)
)
;;;;;;;;;;;;;;;;;;;;;;
;;    Tipo accao    ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun cria-accao(coluna peca)
	(make-accao :coluna-peca (cons coluna peca))

)

(defun accao-coluna(accao)
	(car (accao-coluna-peca accao))

)

(defun accao-peca(accao)
	(cdr (accao-coluna-peca accao))	
)

;;;;;;;;;;;;;;;;;;
;;Tipo Tabuleiro;;
;;;;;;;;;;;;;;;;;;
(defun cria-tabuleiro ()
	(make-tabuleiro :tab (make-array (list *dim-linhas* *dim-colunas*)))
)

;des
(defun copia-tabuleiro (tabuleiro)
	(make-tabuleiro :tab (tabuleiro->array tabuleiro))
)

;des
(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
	(if (not (or (>= linha *dim-linhas*) (>= coluna *dim-colunas*)))
		(aref (tr-tab tabuleiro) linha coluna)
		 T
	)
)

;des
(defun tabuleiro-altura-coluna (tabuleiro coluna)
	(let (
		(altura 0)
		(dim-linhas (array-dimension (tr-tab tabuleiro) 0))
	)
	(loop for i downfrom (1- dim-linhas) downto 0 do
		(if (eq (tabuleiro-preenchido-p tabuleiro i coluna) NIL)
			(incf altura)
			 (return-from tabuleiro-altura-coluna (- dim-linhas altura))
		)
	)
	(- dim-linhas altura))
)

; Talvez seja util depois na parte 2 para as heuristicas...
(defun tabuleiro-altura-agregada (tabuleiro)
	(let(
		(total 0)
		)
		(loop for c from 0 below *dim-colunas* do
			(setf total (+ total (tabuleiro-altura-coluna tabuleiro c)))
		)	
	(* 1 total)
	)
)

(defun tabuleiro-bumpiness (tabuleiro)
	(let(
		(total 0)
		)
		(loop for c from 0 below (1- *dim-colunas*) do
			(setf total (abs (+ total (- (tabuleiro-altura-coluna tabuleiro c) (tabuleiro-altura-coluna tabuleiro (1+ c))))))
		)	
	(* 1 total))	
)

;Nao funciona tenho de rever isto
(defun tabuleiro-buracos (tabuleiro)
	(let((total 0)
		(block NIL)
		)
		(loop for c from 0 below *dim-colunas* do
			(setf block NIL)
			(loop for l downfrom (1- *dim-linhas*) downto 0 do
				(if (eq (tabuleiro-preenchido-p tabuleiro l c) T)
					(setf block T)
				)
				(if (and (eq (tabuleiro-preenchido-p tabuleiro l c) NIL) (eq block T))
					(incf total)
				)
			)
		)
	(* 1 total))
)

(defun tabuleiro-linhas-completas (tabuleiro)
	(let(
		(total 0)
		)
		(loop for l from 0 below (1- *dim-linhas*) do
			(if (eq (tabuleiro-linha-completa-p tabuleiro l) T)
				(incf total)
			)
		)	
	(* 1 total))
)

(defun tabuleiro-linha-completa-p (tabuleiro linha)
	(let (
		(dim-colunas (array-dimension (tr-tab tabuleiro) 1))
	)
	(loop for i from 0 below dim-colunas do
		(if (eq (tabuleiro-preenchido-p tabuleiro linha i) NIL)
			(return-from tabuleiro-linha-completa-p NIL)
		)
	)
	T)
)

(defun tabuleiro-preenche! (tabuleiro linha coluna)
	(if (not (or (> linha (1- *dim-linhas*)) (> coluna (1- *dim-colunas*))))
		(setf (aref (tr-tab tabuleiro) linha coluna) T)
	)
)



(defun tabuleiro-desce-peca (tabuleiro peca coluna)
	(let* (
		(dim-linhas-peca (peca-dimensao-altura peca))
		(drop (- *dim-linhas* dim-linhas-peca))
	)
		(loop for l downfrom drop downto 0 do
			(if (null (tabuleiro-peca-pode-descer tabuleiro peca l coluna))
				(return-from tabuleiro-desce-peca (+ 1 l))
			)
		)
	0)
	
)

(defun tabuleiro-peca-pode-descer(tabuleiro peca linha coluna)
	(let (
	(dim-linhas-peca (peca-dimensao-altura peca))
	(dim-colunas-peca (peca-dimensao-largura peca))
	(_pc 0)
	(_pl 0)
	)
	
		(if (>= (+ linha dim-linhas-peca) *dim-linhas*)
			(return-from tabuleiro-peca-pode-descer T)
		)	
	
		(loop for pl downfrom (1- dim-linhas-peca) downto 0 do
			(loop for pc from 0 below dim-colunas-peca do
				(setf _pl (+ linha pl 1))
				(setf _pc (+ coluna pc))
				(if (and (eq (aref peca pl pc) T) (>= _pl 0))
					(if (not (and (< _pl *dim-linhas*) (null (tabuleiro-preenchido-p tabuleiro (1- _pl) _pc))))
						(return-from tabuleiro-peca-pode-descer NIL)
					)
				)		
			)
		)
		(return-from tabuleiro-peca-pode-descer T)
	)
)

(defun tabuleiro-remove-linha! (tabuleiro linha)
	(loop for i from 0 below *dim-colunas* do
		(setf (aref (tr-tab tabuleiro) linha i) NIL)
	)
	(loop for i from linha below (1- *dim-linhas*) do
		(loop for j from 0 below *dim-colunas* do
			(setf (aref (tr-tab tabuleiro) i j) (aref (tr-tab tabuleiro) (1+ i) j))
		)
	)
	(loop for j from 0 below *dim-colunas* do
		(setf (aref (tr-tab tabuleiro) (1- *dim-linhas*) j) NIL)
	)
)

(defun tabuleiro-topo-preenchido-p (tabuleiro)
	(loop for i from 0 below *dim-colunas* do
		(if (eq (tabuleiro-preenchido-p tabuleiro (1- *dim-linhas*) i) T)
			(return-from tabuleiro-topo-preenchido-p T)
		)
	)
	NIL	
)

(defun tabuleiros-iguais-p (tabuleiro tabuleiro1)
	(equalp tabuleiro tabuleiro1)
)

(defun tabuleiro->array (tabuleiro)
	(let (
		(new-array (make-array (list *dim-linhas* *dim-colunas*)))
	)
	(loop for i from 0 below *dim-linhas* do
		(loop for j from 0 below *dim-colunas* do
			(setf (aref new-array i j) (aref (tr-tab tabuleiro) i j))		
		)
	)
	new-array)	
)

(defun array->tabuleiro (array)
	(let (
		(new-tabuleiro (cria-tabuleiro))
	)
	(loop for i from 0 below *dim-linhas* do
		(loop for j from 0 below *dim-colunas* do
			(setf (aref (tr-tab new-tabuleiro) i j) (aref array i j))		
		)
	)
	new-tabuleiro)
)

;;;;;;;;;;;;;;;;;;
;;  Tipo Estado ;;
;;;;;;;;;;;;;;;;;;

(defun copia-estado (estado)
	(make-estado 
		:pontos (estado-pontos estado) 
		:pecas-por-colocar (copy-seq (estado-pecas-por-colocar estado)) 
		:pecas-colocadas (copy-seq (estado-pecas-colocadas estado)) 
		:tabuleiro (copia-tabuleiro (estado-tabuleiro estado))
	)
)

(defun estado-accao (estado accao linha)
	(loop for i downfrom (1- (array-dimension (accao-peca accao) 0)) downto 0  do
		(loop for j from 0 below (array-dimension (accao-peca accao) 1) do
			(if (eq (aref (accao-peca accao) i j) T)
				(block debug
					(tabuleiro-preenche! (estado-tabuleiro estado) (+ linha i) (+ j (accao-coluna accao)))
				)
			)
		)
	)	
)

(defun estados-iguais-p (estado estado1)
	(equalp estado estado1)
)

(defun estado-final-p (estado)
	(or (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)) (null (estado-pecas-por-colocar estado)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Funcoes do Problema de Procura ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pontos (linhas-removidas)
	(case linhas-removidas 
		((1) 100)
		((2) 200)
		((3) 500)
		((4) 800)
	)
)

(defun solucao (estado)	  
	(estado-final-p estado)
)

(defun accoes (estado)
	(let (
		(peca_cabe 0)
		(lista-accoes NIL)
		(dim-colunas-peca 0)
		)
		(loop for peca in (pecas (car (estado-pecas-por-colocar estado))) do	
			(setf dim-colunas-peca  (peca-dimensao-largura peca))
			(loop for j from 0 below *dim-colunas* do
				(setf peca_cabe 0)
				(loop for h from j below (+ j dim-colunas-peca) do
					(if (null (tabuleiro-preenchido-p 
								(estado-tabuleiro estado) 
								(tabuleiro-altura-coluna (estado-tabuleiro estado) h)
								h)
						)	
						(incf peca_cabe)
					)
				)
				(if (eq peca_cabe dim-colunas-peca)
					(setf lista-accoes (cons (cria-accao j peca) lista-accoes))
				)
			)
		)
		(if (not (null lista-accoes))		
			(return-from accoes (reverse lista-accoes))
		)
	)
)

(defun resultado (estado accao)
	(let (
		(new-estado NIL)
		(linhas-removidas 0)
		(altura (tabuleiro-altura-coluna (estado-tabuleiro estado) (accao-coluna accao)))
		)
		(setf new-estado (copia-estado estado))
		(if (>= (+ altura (peca-dimensao-altura (accao-peca accao))) *dim-linhas*)
			 (estado-accao new-estado accao altura)
			(estado-accao new-estado accao (tabuleiro-desce-peca (estado-tabuleiro new-estado) (accao-peca accao) (accao-coluna accao)))

		)
		;tabuleiro-desce-peca
		;(estado-accao new-estado accao (tabuleiro-altura-coluna (estado-tabuleiro new-estado) (accao-coluna accao)))
		(if (eq (tabuleiro-topo-preenchido-p (estado-tabuleiro new-estado)) NIL)
			(loop for linha from 0 below *dim-linhas* do
				(if (eq (tabuleiro-linha-completa-p (estado-tabuleiro new-estado) (- linha linhas-removidas)) T)
					(block removidas
						(tabuleiro-remove-linha! (estado-tabuleiro new-estado) (- linha linhas-removidas))
						(incf linhas-removidas)
					)
				)
			)
		)
		(if (not (null (estado-pecas-por-colocar estado)))
			(setf (estado-pecas-colocadas new-estado) (cons (car (estado-pecas-por-colocar new-estado)) (estado-pecas-colocadas new-estado)))	
		)
		(setf (estado-pecas-por-colocar new-estado) (cdr (estado-pecas-por-colocar new-estado)))
		(if (not (tabuleiro-topo-preenchido-p (estado-tabuleiro new-estado)))
			(if (not (eq linhas-removidas 0))
				(setf (estado-pontos new-estado) (pontos linhas-removidas))
			)
		)
		new-estado
	)
)

(defun qualidade (estado)
	(* -1 (estado-pontos estado))
)

(defun custo-oportunidade (estado)
	(-  
		(pecas-pontos-maximo (estado-pecas-colocadas estado))
		(estado-pontos estado)
	)
)

(defun formulacao-problema (tabuleiro pecas-por-colocar)
	(return-from formulacao-problema 
		(make-problema 
			:estado-inicial (make-estado :tabuleiro tabuleiro :pecas-por-colocar pecas-por-colocar)
			:solucao #' solucao
			:accoes #' accoes
			:resultado #' resultado
			:custo-caminho #' custo-oportunidade)
	)
)

(defun heuristicas(estado)
	(+ 
		(qualidade estado) 
		(tabuleiro-altura-agregada (estado-tabuleiro estado))
		(tabuleiro-bumpiness (estado-tabuleiro estado))
		;(* -1 (tabuleiro-buracos (estado-tabuleiro estado)))
	)
)

(defun procura-best (tabuleiro pecas-por-colocar)
	(reverse (car (procura-best-aux (cons NIL (make-estado :tabuleiro tabuleiro :pecas-por-colocar pecas-por-colocar)))))
)

(defun procura-best-aux (estado)
	(let* (
			(accoes (accoes (cdr estado)))
			(score NIL)
			(best-score NIL)
			(e-copia NIL)
			(lista_accoes (car estado))
			)
		(block fast
			(loop while (not (null accoes)) do
				;(princ accoes)
				(setf e-copia (resultado (cdr estado) (car accoes)))
				;(princ e-copia)
				
				(if (not (null (cdr accoes)))
					(setf score (cons (cons (car accoes) lista_accoes) e-copia))
					 (setf score (procura-best-aux (cons (cons (car accoes) lista_accoes) e-copia)))	 	
				)

				(if (eq best-score NIL)
					(setf best-score score)
				)
				(if (not (null (cdr score)))
					(block Y
						(format t "~d ~c" (heuristicas (cdr score)) #\linefeed)
						(if (< (heuristicas (cdr score)) (heuristicas (cdr best-score)))
							(block qwerty
								(setf best-score score)
								;(return-from fast)
							)
						)
					)
				)
				
				(setf accoes (cdr accoes))
			)
		)
		(if (not (null best-score))
			(if (null (solucao (cdr best-score)))
				(procura-best-aux best-score)
				best-score
			)
			best-score
		)
	)

)

(load "utils.lisp")
