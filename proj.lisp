 (defparameter *dim-linhas* 18)
 (defparameter *dim-colunas* 10)
 (defparameter *SUPPRESS-SIMILAR-CONSTANT-REDEFINITION-WARNING* T)
 (load "utils.lisp")
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
	pontos
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
	(if (not (or (> linha *dim-linhas*) (> coluna *dim-colunas*)))
		(setf (aref (tr-tab tabuleiro) linha coluna) T)
	)
)

(defun tabuleiro-remove-linha! (tabuleiro linha)
	(loop for i from 0 below *dim-colunas* do
		(setf (aref (tr-tab tabuleiro) linha i) NIL)
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
	(loop for i from 0 below (array-dimension (accao-peca accao) 0) do
		(loop for j from 0 below (array-dimension (accao-peca accao) 1) do
			(tabuleiro-preenche! (estado-tabuleiro estado) (+ linha i) (+ j (accao-coluna accao)))
		)
	)	
)

(defun estados-iguais-p (estado estado1)
	(equalp estado estado1)
)

(defun estado-final-p (estado)
	(or (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)) (null (estado-pecas-por-colocar estado)))
)

;;;;;;;;;;;;;;;;;;;;;;
;;    Tipo peça     ;;
;;;;;;;;;;;;;;;;;;;;;;
(defun pecas(simbolo)
	(cond 
		((eq 'i simbolo) (cons peca-i0 (cons peca-i1 NIL)))
		((eq 'l simbolo) (cons peca-l0 (cons peca-l1 (cons peca-l2 (cons peca-l3 NIL)))))
		((eq 'j simbolo) (cons peca-j0 (cons peca-j1 (cons peca-j2 (cons peca-j3 NIL)))))
		((eq 'o simbolo) (cons peca-i1 NIL))
		((eq 's simbolo) (cons peca-s0 (cons peca-s1 NIL)))
		((eq 'z simbolo) (cons peca-z0 (cons peca-z1 NIL)))
		((eq 't simbolo) (cons peca-t0 (cons peca-t1 (cons peca-t2 (cons peca-t3 NIL)))))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Funcoes do Problema de Procura ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solucao (estado)
	(and (not (tabuleiro-topo-preenchido-p (estado-tabuleiro estado))) (null (estado-pecas-por-colocar estado)))	
)

(defun accoes (estado)
	(let (
		(peca_cabe 0)
		(lista-accoes NIL)
		)
		(loop for i from 0 below *dim-linhas* do
			(loop for peca in (pecas (car (estado-pecas-por-colocar estado))) do	
				
				(loop for j from 0 below *dim-colunas* do
					(setf peca_cabe 0)
					(loop for h from j below (+ j (array-dimension peca 1)) do
						(if (null (tabuleiro-preenchido-p (estado-tabuleiro estado) i h))	
							(incf peca_cabe)
						)
					)
					(if (eq peca_cabe (array-dimension peca 1))
						(setf lista-accoes (cons (cria-accao j peca) lista-accoes))
					)
				)
			)
			(if (not (null lista-accoes))		
				(return-from accoes (reverse lista-accoes))
			)
		)
	)
)

(defun resultado (estado accao)
	(let (
		(new-estado NIL)
		)
		(setf new-estado (copia-estado estado))
		(estado-accao new-estado accao (tabuleiro-altura-coluna (estado-tabuleiro new-estado) (accao-coluna accao)))	
		(if (eq (tabuleiro-topo-preenchido-p (estado-tabuleiro new-estado)) NIL)
			(loop for linha from 0 below *dim-linhas* do
				(if (eq (tabuleiro-linha-completa-p (estado-tabuleiro new-estado) linha) T)
					(tabuleiro-remove-linha! (estado-tabuleiro new-estado) linha)
				)
				(setf (estado-pecas-colocadas new-estado) (cons (car (estado-pecas-por-colocar new-estado)) (estado-pecas-colocadas new-estado)))
				(setf (estado-pecas-por-colocar new-estado) (cdr (estado-pecas-por-colocar new-estado)))
			)
		)
	new-estado)
)

(defun formulacao-problema (tabuleiro pecas-por-colocar)
	(return-from formulacao-problema 
		(make-problema :estado-inicial (make-estado :tabuleiro tabuleiro :pecas-por-colocar pecas-por-colocar)
			:solucao #' solucao
			:accoes #' accoes
			:resultado #' resultado
			:custo-caminho NIL)
	)
)