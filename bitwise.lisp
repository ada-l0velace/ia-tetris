(defparameter *dim-linhas* 18)
(defparameter *dim-colunas* 10)
(defparameter *grid-mask* (ash 1 *dim-colunas*))
(defconstant *tabuleiro-mascaras* #(512 256 128 64 32 16 8 4 2 1))
(defstruct tabuleiro (:conc-name tr-)
	tab
	alturas
)

(defun logbit (integer index)
	(ldb (byte 1 index) integer))
 
(defun cria-tabuleiro ()
	(make-tabuleiro 
		:tab (make-array (list *dim-linhas*) :initial-element 0)
		:alturas (make-array (list *dim-colunas*) :initial-element 0)
	)
)

(defun copia-tabuleiro (tabuleiro)
	(make-tabuleiro 
		:tab (tabuleiro->array tabuleiro)
		:alturas (copy-seq (tr-alturas tabuleiro))
	)
)

(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
	(if (> (logbit (aref (tabuleiro-tab tabuleiro) linha) (- (1- *dim-colunas*) coluna)) 0)
		T
		NIL
	)
)

(defun tabuleiro-linha-p (tabuleiro linha)
	(aref (tabuleiro-tab tabuleiro) linha))



(defun tabuleiro-preenche! (tabuleiro linha coluna)
	(when (and (<= 0 linha (1- *dim-linhas*)) (<= 0 coluna (1- *dim-colunas*)))
		(setf (aref (tabuleiro-tab tabuleiro) linha) (logior (aref (tabuleiro-tab tabuleiro) linha) (aref *tabuleiro-mascaras* coluna)))
		(when (> (1+ linha) (tabuleiro-altura-coluna tabuleiro coluna))
			(tabuleiro-altura! tabuleiro coluna (1+ linha))
		)
	)
T)

(defun tabuleiro-muda-ponto! (tabuleiro linha coluna valor) 
	(if (eq valor T)
		(tabuleiro-preenche! tabuleiro linha coluna)
		(tabuleiro-apaga! tabuleiro linha coluna)
	)
T)

(defun tabuleiro-apaga! (tabuleiro linha coluna)
	(when (and (< 0 linha (1- *dim-linhas*)) (< 0 coluna (1- *dim-colunas*)))
		(setf (aref (tabuleiro-tab tabuleiro) linha) (logand (aref (tabuleiro-tab tabuleiro) linha) (lognot (aref *tabuleiro-mascaras* coluna)))) 
		(when (<= linha (tabuleiro-altura-coluna tabuleiro coluna))			
			(tabuleiro-altura! tabuleiro coluna (tabuleiro-calcula-altura tabuleiro coluna linha))
		)
	)
T)

(defun tabuleiro-altura-coluna (tabuleiro coluna)
	(aref (tr-alturas tabuleiro) coluna))

(defun tabuleiro-altura! (tabuleiro coluna altura)
	(setf (aref (tr-alturas tabuleiro) coluna) altura)
T)

(defun tabuleiro-calcula-altura (tabuleiro coluna topo)
	(let (
		(altura 0)
	)
	(loop for i downfrom topo downto 0 do
		(if (null (tabuleiro-preenchido-p tabuleiro i coluna))
			(incf altura)
			(return-from tabuleiro-calcula-altura (1+ (- topo altura)))
		)
	)
	0) ; coluna vazia
)

;; tabuleiro-desce-peca: tabuleiro x peca x coluna --> inteiro
;; esta funcao calcula a altura ate onde a peca pode descer ate encontrar o chao
;; ou um bloco preenchido onde pousar
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

;; tabuleiro-peca-pode-descer-p: tabuleiro x peca x linha x coluna --> logico
;; esta funcao recebe um tabuleiro, uma peca, a linha e uma coluna e verifica
;; se ao longo da sua largura a peca pode descer ou nao retorna verdadeiro se pode
;; retorna falso se nao
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

(defun tabuleiros-iguais-p (tabuleiro tabuleiro1)
	(equalp tabuleiro tabuleiro1)	
)

;; tabuleiro->array: tabuleiro --> array
;; este transformador de saida recebe um tabuleiro e devolve um novo array igual ao array do tabuleiro recebido
(defun tabuleiro->array (tabuleiro)
	(let (
		(new-array (make-array (list *dim-linhas*) :initial-element 0))
	)
	(loop for i from 0 below *dim-linhas* do
		(loop for j from 0 below *dim-colunas* do
			(if (tabuleiro-preenchido-p tabuleiro i j)
				(setf (aref new-array i j) (aref (tr-tab tabuleiro) i j))
			)		
		)
	)
	new-array)	
)

;; array->tabuleiro: array --> tabuleiro
;; este transformador de saida recebe um array e controi um novo tabuleiro com o conteudo do array recebido e
;; devolve um tabuleiro 
(defun array->tabuleiro (array)
	(let (
		(new-tabuleiro (cria-tabuleiro))
	)
	(loop for i from 0 below *dim-linhas* do
		(loop for j from 0 below *dim-colunas* do
			(if (aref array i j)
				(tabuleiro-muda-ponto! new-tabuleiro i j (aref array i j))		
			)
		)
	)
	new-tabuleiro)
)

(defun tabuleiro-buracos (tabuleiro)
	(let (
		(under-mask 0)
		(l-neighbor-mask 0)
		(r-neighbor-mask 0)
		(found-holes 0)
		(line 0)
		(filled 0)
		(min-y 0)
		)

		(loop while 
			(and 
				(< min-y *dim-linhas*) 
				(eq (tabuleiro-linha-p tabuleiro min-y) *grid-mask*)
			) 
		do 
			(incf min-y)
		)
		;(format T "~d~c" grid-mask #\linefeed)
		(loop for y from min-y below *dim-linhas* do
			(setf line (tabuleiro-linha-p tabuleiro y))
			(setf filled (logand (lognot line) *grid-mask*))
			;(format T "~d~c" under-mask #\linefeed)
			(setf under-mask (logior under-mask filled))
			;(format T "~d~c" filled #\linefeed)
			;(format T "~d~c" under-mask #\linefeed)
			(setf l-neighbor-mask (logior l-neighbor-mask (ash filled 1)))
			(setf r-neighbor-mask (logior r-neighbor-mask (ash filled -1)))
			;(format T "~d~c" (integer-length (logand r-neighbor-mask line)) #\linefeed)
			(incf found-holes 
				(+
					(integer-length (logand under-mask line))
					(integer-length (logand l-neighbor-mask line))
					(integer-length (logand r-neighbor-mask line))
				)
			)
		)
	found-holes)
)