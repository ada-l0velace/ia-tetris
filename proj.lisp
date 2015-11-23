; Ricardo Campos 76398
; Daniel Leitao 77939
; Pedro Cabral 77968

(defparameter *dim-linhas* 18)
(defparameter *dim-colunas* 10)
(defparameter *SUPPRESS-SIMILAR-CONSTANT-REDEFINITION-WARNING* T)
 
;; Tipo accao

; (defstruct (accao)
; 	coluna-peca
; )

;; Tipo tabuleiro

(defstruct (tabuleiro (:conc-name tr-))
	tab
	alturas
	alturas-rel
)

;; Tipo Estado
(defstruct (estado 
	)
	(pontos 0 :type integer)
	pecas-por-colocar
	pecas-colocadas
	tabuleiro
)

;; Tipo Node
;; estado-actual - estado actual do node
;; pai - corresponde ao pai do node
;; accao - accao que foi aplicada ao estado
;; profundidade - em que profundidade foi aplicada
;; peso - corresponde ao peso nas procuras ex: f(n) = g(n) + h(n)
(defstruct node
	estado-actual
	pai
	accao
	(profundidade 0 :type integer)
	(peso 0.0)
)

;; Tipo Problema
;; estado-inicial - estado inicial do problema
;; solucao - funcao que verifica se chegou ao objectivo
;; accoes - funcao que calcula todas as accoes possiveis de um estado
;; resultado - funcao que aplica uma accao a um estado e devolve a copia 
;; custo-caminho - funcao de custo de todas as accoes realizadas ate ao momento
(defstruct problema 
	estado-inicial
	solucao
	accoes
	resultado
	custo-caminho
)


;;;;;;;;;;;;;;;;;;;;;;
;;    Tipo peca     ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun peca-dimensao-largura (peca)
	(array-dimension peca 1)
)
(defun peca-dimensao-altura (peca)
	(array-dimension peca 0)
)

(defun peca-preenchido-p (peca linha coluna)
	(aref peca linha coluna)
)

(defun ppm (peca) ; peca-pontos-maximo -- n usar em mais nenhum sitio
	(cond 
		((eq 'i peca) 800)
		((eq 'l peca) 500)
		((eq 'j peca) 500)
		((eq 'o peca) 300)
		((eq 's peca) 300)
		((eq 'z peca) 300)
		((eq 't peca) 300)
	)
)

(defun pecas-pontos-maximo (pecas)
	(let (
		(total 0)
		)
		(loop for peca in pecas do
			(incf total (ppm peca))
		)
	total)
)
;;;;;;;;;;;;;;;;;;;;;;
;;    Tipo accao    ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun cria-accao(coluna peca)
	(cons coluna peca))

(defun accao-coluna(accao)
	(car accao)

)

(defun accao-peca(accao)
	(cdr accao)	
)

;;;;;;;;;;;;;;;;;;
;;Tipo Tabuleiro;;
;;;;;;;;;;;;;;;;;;
(defun cria-tabuleiro ()
	(make-tabuleiro :tab (make-array (list *dim-linhas* *dim-colunas*))
					:alturas (make-array (list *dim-colunas*) :initial-element 0)
					:alturas-rel (make-array (list *dim-colunas*) :initial-element 0)
					)
)

;des
(defun copia-tabuleiro (tabuleiro)
	(make-tabuleiro :tab (tabuleiro->array tabuleiro)
					:alturas (copy-seq (tr-alturas tabuleiro))
					:alturas-rel (copy-seq (tr-alturas-rel tabuleiro))
	)
)

;des
(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
	(if (not (or (>= linha *dim-linhas*) (>= coluna *dim-colunas*)))
		(aref (tr-tab tabuleiro) linha coluna)
		 T
	)
)


(defun tabuleiro-calcula-altura (tabuleiro coluna topo)
	(let (
		(altura 0)
	)
	(loop for i downfrom topo downto 0 do
		(if (eq (tabuleiro-preenchido-p tabuleiro i coluna) NIL)
			(incf altura)
			(return-from tabuleiro-calcula-altura (1+ (- topo altura)))
		)
	)
	0) ; coluna vazia
)

(defun tabuleiro-altura-coluna (tabuleiro coluna)
  (aref (tr-alturas tabuleiro) coluna)
)


;define altura de uma coluna no tabuleiro
(defun tabuleiro-altura! (tabuleiro coluna altura)
	(setf (aref (tr-alturas tabuleiro) coluna) altura)
T)

(defun tabuleiro-altura-rel (tabuleiro coluna)
	(aref (tr-alturas-rel tabuleiro) coluna)
)

;define altura relativa de uma coluna no tabuleiro
(defun tabuleiro-altura-rel! (tabuleiro coluna altura)
	(setf (aref (tr-alturas-rel tabuleiro) coluna) altura)
)

; actualiza a altura relativa da coluna 'a direita da indicada
(defun tabuleiro-altura-rel-actualiza! (tabuleiro coluna)
	(if (not (eq coluna (1- *dim-colunas*))) ; se nao for a ultima coluna
		(tabuleiro-altura-rel! tabuleiro (1+ coluna)
			(-
				(tabuleiro-altura-coluna tabuleiro (1+ coluna))
				(tabuleiro-altura-coluna tabuleiro coluna))))
)


; Talvez seja util depois na parte 2 para as heuristicas...
(defun tabuleiro-altura-agregada (tabuleiro)
	(reduce #' + (tr-alturas tabuleiro))
)

(defun tabuleiro-bumpiness (tabuleiro)
	(let(
		(total 0)
		)
		(loop for c from 0 below (1- *dim-colunas*) do
			(setf total (abs (+ total (-
										(tabuleiro-altura-coluna tabuleiro c)
										(tabuleiro-altura-coluna tabuleiro (1+ c))
										))))
		)	
	(* 1 total)
	)	
)

;Nao funciona tenho de rever isto
(defun tabuleiro-buracos (tabuleiro)
	(let((total 0)
		(block NIL)
		)
		(loop for c from 0 below *dim-colunas* do
			(setf block NIL)
			(loop for l downfrom (tabuleiro-altura-coluna tabuleiro c) downto 0 do
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

(defun tabuleiro-linhas-completas (tabuleiro) ; pode ser optimizado
	(let(
		(total 0)
		)
		(loop for l from 0 below (1- *dim-linhas*) do
			(if (tabuleiro-linha-completa-p tabuleiro l)
				(incf total)
			)
		)	
	total)
)

(defun tabuleiro-linha-completa-p (tabuleiro linha) ; pode ser optimizado
	(loop for i from 0 below *dim-colunas* do
		(if (null (tabuleiro-preenchido-p tabuleiro linha i))
			(return-from tabuleiro-linha-completa-p NIL)
		)
	)
	T
)




(defun tabuleiro-preenche! (tabuleiro linha coluna) ; nao usar directamente
	(if (not (or (> linha (1- *dim-linhas*)) (> coluna (1- *dim-colunas*))))
		(block actualiza
			(setf (aref (tr-tab tabuleiro) linha coluna) T)

			(if (> (1+ linha) (tabuleiro-altura-coluna tabuleiro coluna))
				(block altura
					(tabuleiro-altura! tabuleiro coluna (1+ linha))
					;(tabuleiro-altura-rel-actualiza! tabuleiro coluna)
				)
				
			)
		)
	)
T)


(defun tabuleiro-muda-ponto! (tabuleiro linha coluna valor) ; valor T para preencher, NIL para apagar
	(if (eq valor T)
		(tabuleiro-preenche! tabuleiro linha coluna)
		(tabuleiro-apaga! tabuleiro linha coluna)
	)
T)

(defun tabuleiro-apaga! (tabuleiro linha coluna) ; nao usar directamente
	(if (not (or (> linha (1- *dim-linhas*)) (> coluna (1- *dim-colunas*))))
		(block actualiza
			(setf (aref (tr-tab tabuleiro) linha coluna) NIL)
			(if (<= linha (tabuleiro-altura-coluna tabuleiro coluna))			
				(block altura
					(tabuleiro-altura! tabuleiro coluna (tabuleiro-calcula-altura tabuleiro coluna linha))
				)
			)
		)
	)
T)



(defun tabuleiro-desce-peca (tabuleiro peca coluna)
	(let* (
		(dim-linhas-peca (peca-dimensao-altura peca))
		(drop (- *dim-linhas* dim-linhas-peca))
	)
		(loop for l downfrom drop downto 0 do
			(if (null (tabuleiro-peca-pode-descer-p tabuleiro peca l coluna))
				(return-from tabuleiro-desce-peca (+ 1 l))
			)
		)
	0)
	
)

(defun tabuleiro-peca-pode-descer-p (tabuleiro peca linha coluna)
	(let (
		(dim-linhas-peca (peca-dimensao-altura peca))
		(dim-colunas-peca (peca-dimensao-largura peca))
		(_pc 0)
		(_pl 0)
	)
	
		(if (>= (+ linha dim-linhas-peca) *dim-linhas*)
			(return-from tabuleiro-peca-pode-descer-p T)
		)	
	
		(loop for pl downfrom (1- dim-linhas-peca) downto 0 do
			(loop for pc from 0 below dim-colunas-peca do
				(setf _pl (+ linha pl 1))
				(setf _pc (+ coluna pc))
				(if (and (eq (aref peca pl pc) T) (>= _pl 0))
					(if (not (and (< _pl *dim-linhas*) (null (tabuleiro-preenchido-p tabuleiro (1- _pl) _pc))))
						(return-from tabuleiro-peca-pode-descer-p NIL)
					)
				)		
			)
		)
		(return-from tabuleiro-peca-pode-descer-p T)
	)
)

(defun tabuleiro-remove-linha! (tabuleiro linha) ; pode ser optimizado, removendo varias
	(loop for i from linha below (1- *dim-linhas*) do
		(loop for j from 0 below *dim-colunas* do
			(tabuleiro-muda-ponto! tabuleiro i j (tabuleiro-preenchido-p tabuleiro (1+ i) j))
		)
	)
	(loop for j from 0 below *dim-colunas* do ; linha mais acima
		(tabuleiro-muda-ponto! tabuleiro (1- *dim-linhas*) j NIL)
	)
T)

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
			(tabuleiro-muda-ponto! new-tabuleiro i j (aref array i j))		
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
					(tabuleiro-muda-ponto! (estado-tabuleiro estado) (+ linha i) (+ j (accao-coluna accao)) T)
				)
			)
		)
	)	
)

(defun estados-iguais-p (estado estado1)
	(and 
		(tabuleiros-iguais-p (estado-tabuleiro estado) (estado-tabuleiro estado1))
		(eq (estado-pontos estado) (estado-pontos estado1))
		(equal (estado-pecas-colocadas estado) (estado-pecas-colocadas estado1))
		(equal (estado-pecas-por-colocar estado) (estado-pecas-por-colocar estado1))	
	)
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
		((2) 300)
		((3) 500)
		((4) 800)
	)
)

(defun solucao (estado)
	(and (null (tabuleiro-topo-preenchido-p (estado-tabuleiro estado))) (null (estado-pecas-por-colocar estado)))
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
				(loop for h from j below (min (+ j dim-colunas-peca) *dim-colunas*) do
					(if (and 
							(null 
								(tabuleiro-preenchido-p 
									(estado-tabuleiro estado) 
									(tabuleiro-altura-coluna (estado-tabuleiro estado) h)
									h)
							)
							(null (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)))	
						)
						(incf peca_cabe)
					)
				)
				(if (eq peca_cabe dim-colunas-peca)
					(setf lista-accoes (cons (cria-accao j peca) lista-accoes))
				)
			)
		)
		(if lista-accoes		
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
		(if (>= (+ (1- altura) (peca-dimensao-altura (accao-peca accao))) *dim-linhas*)
			 (estado-accao new-estado accao altura)
			(estado-accao new-estado accao (tabuleiro-desce-peca (estado-tabuleiro new-estado) (accao-peca accao) (accao-coluna accao)))

		)
		;tabuleiro-desce-peca
		;(estado-accao new-estado accao (tabuleiro-altura-coluna (estado-tabuleiro new-estado) (accao-coluna accao)))
		
		(if (null (tabuleiro-topo-preenchido-p (estado-tabuleiro new-estado)))
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
		(if (not (eq linhas-removidas 0))
			(incf (estado-pontos new-estado) (pontos linhas-removidas))
		)
		new-estado
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Tipo Node ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun cria-node-filho (problema pai accao &optional (heuristica (lambda (a) (declare (ignore a)) 0)))
	(let (
		(estado (funcall (problema-resultado problema) 
				(node-estado-actual pai)
				accao))
		)
		(make-node
			:estado-actual estado 
			:pai pai
			:accao accao
			:profundidade (1+ (node-profundidade pai))
			:peso (+
				(funcall (problema-custo-caminho problema) estado)
				(funcall heuristica estado))
		)
	)
	
)

(defun nodes-iguais-p (node1 node2)
	(and 
		(estados-iguais-p (node-estado-actual node1) (node-estado-actual node2))
		(equalp (node-accao node1) (node-accao node2))
		(eq (node-profundidade node1) (node-profundidade node2))
		(eq (node-peso node1) (node-peso node1))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Heuristicas ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun linhas-completas (estado)
	(* 1 (tabuleiro-linhas-completas (estado-tabuleiro estado)))
)

(defun altura-agregada (estado)
	(* 0.3 (tabuleiro-altura-agregada (estado-tabuleiro estado)))
)

(defun bumpiness (estado)
	(* 1 (tabuleiro-bumpiness (estado-tabuleiro estado)))
)

(defun qualidade (estado)
	(* -1 (estado-pontos estado))
)

(defun custo-oportunidade (estado)
	(* 1 (-  
	 	(pecas-pontos-maximo (estado-pecas-colocadas estado))
	 	(estado-pontos estado)
	))
)

(defun custo-oportunidade2 (estado) ; assume que todas as pecas teem 4 blocos
	(-  
		(*
			(/ (length (estado-pecas-colocadas estado)) *dim-colunas*)
			(pontos 4)
		)
		(estado-pontos estado)
	)
)

(defun heuristicas(estado)
	(+ 
		;(linhas-completas estado)
		;(custo-oportunidade estado)
		(altura-agregada estado)
		(bumpiness estado)
		(qualidade estado)
		(tabuleiro-buracos (estado-tabuleiro estado))
	)
)

(defun formulacao-problema (tabuleiro pecas-por-colocar)
	(make-problema 
			:estado-inicial (make-estado :tabuleiro tabuleiro :pecas-por-colocar pecas-por-colocar)
			:solucao #' solucao
			:accoes #' accoes
			:resultado #' resultado
			:custo-caminho #' custo-oportunidade)
)

;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Procuras ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;



(defun procura-pp (problema)
	(let (
		(solucao (depth-first-search 
			problema
			(make-node 
					:estado-actual (problema-estado-inicial problema)))
		))
		(procura-get-solucao solucao))
)

(defun depth-first-search (problema node)
	(let (
		(accoes NIL)
		(new-node NIL)
		(result NIL))
		(if (funcall (problema-solucao problema) (node-estado-actual node))
		 	(return-from depth-first-search node)
		)
		(setf accoes (reverse (funcall (problema-accoes problema) (node-estado-actual node))))
		(loop for accao in accoes do
			(setf new-node (cria-node-filho problema node accao))
		 	(setf result (depth-first-search problema new-node))	
			(if result
			 	(if (funcall (problema-solucao problema) (node-estado-actual result))
			 		(return-from depth-first-search result)		
				)		
			)
		)
		result)
)

(defun procura-A* (problema heuristica)
	(let (
		(solucao (a-star-search 
			problema
			(make-node 
					:estado-actual (problema-estado-inicial problema)
					:pai NIL
					:profundidade 0
					:accao NIL
					:peso (funcall heuristica (problema-estado-inicial problema)))
			heuristica
		)))
		(procura-get-solucao solucao))
)

(defun a-star-search (problema node heuristica)
	(let ((open (make-instance 'binary-heap))
		(current NIL)
		(accoes NIL)
		(new-node NIL)
		)
		(insert_heap open (node-peso node) node)
		(loop while (> (heap-size open) 0) do
			
			(setf current (extract-min open))
			;(format t "~d ~c"  current #\linefeed)
			(if (funcall (problema-solucao problema) (node-estado-actual current))
				(return-from a-star-search current)
			)
			(setf accoes (funcall (problema-accoes problema) (node-estado-actual current)))
			(loop for accao in accoes do
				(block continue
					(setf new-node (cria-node-filho problema current accao heuristica))

					(insert_heap open (node-peso new-node) new-node)
				)	
			)
			;(format t "---------------------------- ~c" #\linefeed)
		)
	)
	NIL
)


(defun procura-best (array pecas-por-colocar)
	(let* (
		(tabuleiro (array->tabuleiro array))
		(problema (formulacao-problema tabuleiro pecas-por-colocar))
		(estado-inicial (make-estado :tabuleiro tabuleiro :pecas-por-colocar pecas-por-colocar))
		(solucao (a-star-search 
			problema
			(make-node 
				:estado-actual estado-inicial
				:pai NIL
				:profundidade 0
				:accao NIL
				:peso (heuristicas estado-inicial)
			)
			#'heuristicas))
		)
		
	(procura-get-solucao solucao))
)

(defun procura-best-aux (problema node f_limit)
	(let (
			(accoes (funcall (problema-accoes problema) (node-estado-actual node)))
			(score NIL)
			(best-score NIL)
		)
		(loop while (not (null accoes)) do
			(setf score (cria-node-filho problema node (car accoes) #' heuristicas))
			(setf (node-peso score) (max (node-peso score) (node-peso node)))
			(if (eq best-score NIL)
				(setf best-score score)
				(if (not (null score))
					(if (< (node-peso score) (node-peso best-score))
							(setf best-score score)
					)
				)
			)	
			(setf accoes (cdr accoes))
		)
		(if (not (null best-score))
		 	(if (null (funcall (problema-solucao problema) (node-estado-actual best-score)))
				(block not_solution
					(procura-best-aux problema best-score (min (node-peso best-score) f_limit))
				)
				best-score
			)
			node	
		)
	)
)

(defun procura-get-solucao (solucao)
	(let (
		(lista-accoes NIL))
		(if (null solucao)
			(return-from procura-get-solucao solucao)
		)
		(loop while (not (null (node-accao solucao))) do
			(push (node-accao solucao) lista-accoes)
			(setf solucao (node-pai solucao))
		)
	lista-accoes)	
)

(load (compile-file "utils.lisp")) 
;(load "utils.fas")

(defun pecas(simbolo)
	(cond 
		((eq 'i simbolo) (list peca-i0 peca-i1)) 
		((eq 'l simbolo) (list peca-l0 peca-l1 peca-l2 peca-l3))
		((eq 'j simbolo) (list peca-j0 peca-j1 peca-j2 peca-j3))
		((eq 'o simbolo) (list peca-o0))
		((eq 's simbolo) (list peca-s0 peca-s1))
		((eq 'z simbolo) (list peca-z0 peca-z1))
		((eq 't simbolo) (list peca-t0 peca-t1 peca-t2 peca-t3))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;Priority Queues;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;listaS ordenada;;;;;;;;;;;;;;;;
;;; SELECT-BEST chooses a node in step 3...
(defun select-best (lst)
	(first lst)
)
;;; INSERT puts NODE onto LST, which is ordered
;;; by FVALUE.
(defun insert_lst (node lst)
	(cond ((null lst)(list node))
		((< (node-peso node) (node-peso (first lst)))
			(cons node lst))
		(t 
			(cons (first lst) (insert_lst node (rest lst))))
	)
)	

;;;;;;;;;;;;;binary min-heap;;;;;;;;;;;;;;;; 
(deftype array-index () `(integer 0 ,(1- array-dimension-limit)))

(defconstant +initial-size+ 50 "initial queue vector size")



(defstruct (node_bh (:constructor %make-node_bh (key data index)))
  (key 0 )
  (index 0 :type array-index)
  (data nil))

(defclass binary-heap ()
  ((array :accessor bin-heap-array
          :type (vector (or null node))
          :initarg :array
          :initform (make-array +initial-size+
                                :adjustable t 
                                :fill-pointer 0 
                                :element-type '(or null node)
                                :initial-element nil))))



;(declaim (inline parent left right %make-node))

(defun parent (k)
  (declare (type array-index k))
  (floor (1- k) 2))

(defun left (k)
  (declare (type (integer 0 #.(floor array-dimension-limit 2)) k))
  (1+ (* k 2)))

(defun right (k)
  (declare (type (integer 0 #.(floor array-dimension-limit 2)) k))
  (* (1+ k) 2))

(defun peek-min (heap)
  (let ((node (aref (bin-heap-array heap) 0)))
    (values (node_bh-data node)
            (node_bh-key node))))
(defun peek-min-node (heap)
  (let ((node (aref (bin-heap-array heap) 0)))
    node))


(defun heap-contains (heap node)
	(if (< (node_bh-index node) (heap-size heap))
		(equalp 
			(aref (bin-heap-array heap) (node_bh-index node))
			node
		)
		NIL
	)
)

(defun clear-heap (heap)
  (setf (fill-pointer (bin-heap-array heap)) 0))

(defun empty-p (heap)
  (zerop (fill-pointer (bin-heap-array heap))))

(defun heap-size (heap)
  (length (bin-heap-array heap)))

(defun extract-min (heap)
  (let ((array (bin-heap-array heap))
	(node (aref (bin-heap-array heap) 0)))
    (assert node)
    (setf (aref array 0) (aref array (1- (length array)))
          (aref array (1- (length array))) nil)
    (when (> (decf (fill-pointer array)) 1)
      (sink array 0))
    (values (node_bh-data node)
            (node_bh-key node))))

;(declaim (inline swap-nodes))
(defun swap-nodes (array i j)
  (declare (type array-index i j))
  (setf (node_bh-index (aref array i)) j
        (node_bh-index (aref array j)) i)
  (rotatef (aref array i) (aref array j)))

(defun sink (array index)
  (let ((maxindex (1- (length array))))
    (if (zerop maxindex)
        maxindex
        (loop for i = index then j
              with j = 0
              while (<= (left i) maxindex) do
                (cond
                  ((< maxindex (right i))
                   (setf j (left i)))
                  ((< (node_bh-key (aref array (left i)))
                       (node_bh-key (aref array (right i))))
                   (setf j (left i)))
                  (t
                   (setf j (right i))))
                (when (< (node_bh-key (aref array i))
                          (node_bh-key (aref array j)))
                  (loop-finish))
                (swap-nodes array i j)
              finally (return array)))))

(defun perlocate-up (array vindex)
  (loop for index = vindex then parent
        for parent = (parent index)
        with key = (node_bh-key (aref array vindex))
        while (and (>= parent 0)
                   (<= key (node_bh-key (aref array parent))))
        do (swap-nodes array index parent) 
        finally (return (aref array index))))

(defun insert_heap (heap key data)
  (let ((node (%make-node_bh key data 0))
        (array (bin-heap-array heap)))
    (perlocate-up array (setf (node_bh-index node) 
                              (vector-push-extend node array)))))