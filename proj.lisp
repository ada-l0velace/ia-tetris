; Ricardo Campos 76398
; Daniel Leitao 77939
; Pedro Cabral 77968

(defparameter *dim-linhas* 18)
(defparameter *dim-colunas* 10)
(defparameter *hash-accoes* (make-hash-table))
(defparameter *grid-mask* (1- (ash 1 *dim-colunas*)))
(defconstant *tabuleiro-mascaras* #(512 256 128 64 32 16 8 4 2 1))


;; Tipo tabuleiro
;; tab - array do tabuleiro 1 dimensao cada numero na base decimal corresponde uma coluna
;; alturas - array das alturas de cada coluna do tabuleiro
(defstruct (tabuleiro (:conc-name tr-))
	tab
	alturas
)

;; Tipo Estado
;; pontos -
;; pecas-por-colocar -
;; pecas-colocadas -
;; tabuleiro -
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
	(custo-caminho (lambda (a) (declare (ignore a)) 0))
)

(defun logbit (integer index)
	(ldb (byte 1 index) integer))

;;;;;;;;;;;;;;;;;;;;;;
;;    Tipo peca     ;;
;;;;;;;;;;;;;;;;;;;;;;

;; peca-dimensao-largura: peca --> inteiro
;; funcao que recebe uma peca e retorna a largura dela
(defun peca-dimensao-largura (peca)
	(array-dimension peca 1)
)

;; peca-dimensao-altura: peca --> inteiro
;; funcao que recebe uma peca e retorna a altura dela
(defun peca-dimensao-altura (peca)
	(array-dimension peca 0)
)

;; peca-preenchido-p: peca x linha x coluna --> logico
;; funcao que verifica se uma posicao na peca esta preenchida
(defun peca-preenchido-p (peca linha coluna)
	(aref peca linha coluna)
)

;; peca-tabuleiro-coluna-altura peca x tabuleiro x coluna --> inteiro
;; funcao que calcula qual e a altura maxima no tabuleiro no sito onde a peca vai cair
(defun peca-tabuleiro-coluna-altura (peca tabuleiro coluna)
	(let( 
		(peca-l (peca-dimensao-largura peca))
		(altura-actual 0)
		(max-coluna *dim-linhas*)
		)
		(loop for i from 0 below peca-l do
			(setf altura-actual (tabuleiro-altura-coluna tabuleiro (+ i coluna)))
			(if (< altura-actual max-coluna)
				(setf max-coluna altura-actual)
			)
		)	
	max-coluna)
)

;; peca-pontos-maximo: peca --> inteiro
;; funcao que recebe uma peca e retorna o valor maximo que se pode obter com esta peca em uma jogada
(defun peca-pontos-maximo (peca) 
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

;; peca-pontos-maximo: lista x pecas --> inteiro
;; funcao que recebe uma lista de pecas e retorna um inteiro correspondente ao valor total de pontos
;; das pecas ja jogadas no tabuleiro
(defun pecas-pontos-maximo (pecas)
	(let (
		(total 0)
		)
		(loop for peca in pecas do
			(incf total (peca-pontos-maximo peca))
		)
	total)
)

;; peca-pontos-maximo: lista x pecas --> inteiro
;; funcao que recebe uma lista de pecas e retorna um inteiro correspondente ao valor total de pontos
;; da ultima peca jogada no tabuleiro
(defun pecas-pontos-maximo-limitado (pecas)
	(let (
		(total 0)
		(limit 0)
		)
		(block break
			(loop for peca in pecas do
				(if (eq limit 1)
					(return-from break)
				)
				(incf total (peca-pontos-maximo peca))
				(incf limit)
			)
		)
	total)
)

;; pontos: inteiro --> inteiro
;; calcula o numero de pontos obtido com o numero de linhas removidas nessa jogada
(defun pontos (linhas-removidas)
	(case linhas-removidas 
		((1) 100)
		((2) 300)
		((3) 500)
		((4) 800)
	)
)

;; peca-tabuleiro-accoes: peca x tabuleiro x lista --> lista accoes
;; recebe uma peca, um tabuleiro e uma lista e retorna a lista de accoes que foi efectuado
;; concatenando com lista recebida por parametro
(defun peca-tabuleiro-accoes (peca tabuleiro lista-accoes)
	(let (
		(peca_cabe 0)
		(dim-colunas-peca 0)
		)
		(setf dim-colunas-peca  (peca-dimensao-largura peca))
		(if (tabuleiro-topo-preenchido-p tabuleiro)
			(return-from peca-tabuleiro-accoes NIL)
		)
		(loop for j from 0 below *dim-colunas* do
			(setf peca_cabe 0)
			(loop for h from j below (min (+ j dim-colunas-peca) *dim-colunas*) do
				(if (null 
						(tabuleiro-preenchido-p 
						tabuleiro 
						(tabuleiro-altura-coluna tabuleiro h)
						h)
					)
					(incf peca_cabe)
				)
			)
			(if (eq peca_cabe dim-colunas-peca)
				(setf lista-accoes (cons (cria-accao j peca) lista-accoes))
			)
		)
	lista-accoes)
)

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

;;;;;;;;;;;;;;;;;;;;;;
;;    Tipo accao    ;;
;;;;;;;;;;;;;;;;;;;;;;

;; cria-accao: inteiro x array --> accao
;; este constructor recebe um inteiro correspondente a posicao da coluna mais a esquerda a partir
;; da qual a peca vai ser colocada e um array com a configuracao da peca a colocar e devolve uma
;; nova accao
(defun cria-accao(coluna peca)
	(cons coluna peca))

;; accao-coluna: accao x inteiro --> inteiro
;; este selector devolve um inteiro correspondente a coluna mais a esquerda a partir da qual a
;; peca vai ser colocada
(defun accao-coluna(accao)
	(car accao)

)

;; accao-peca: accao --> array
;; este selector devolve o array com a configuracao geometrica exacta com que vai ser colocada
(defun accao-peca(accao)
	(cdr accao)	
)

;;;;;;;;;;;;;;;;;;
;;Tipo Tabuleiro;;
;;;;;;;;;;;;;;;;;;

;; cria-tabuleiro: {} --> tabuleiro
;; este construtor nao recebe qualquer argumento e devolve um novo tabuleiro vazio
(defun cria-tabuleiro ()
	(make-tabuleiro 
		:tab (make-array (list *dim-linhas*) :initial-element 0)
		:alturas (make-array (list *dim-colunas*) :initial-element 0)
	)
)	

;; copia-tabuleiro: tabuleiro -> tabuleiro
;; este construtor recebe um tabuleiro e devolve um novo tabuleiro com o mesmo conteudo do tabuleiro
;; recebido
(defun copia-tabuleiro (tabuleiro)
	(make-tabuleiro 
		:tab (copy-seq (tr-tab tabuleiro))
		:alturas (copy-seq (tr-alturas tabuleiro))
	)
)

;; tabuleiro-preenchido-p: tabuleiro x inteiro x inteiro --> logico
;; este selector recebe um tabuleiro um inteiro correspondente ao numero de linha e um inteiro
;; correspondente ao numero da coluna e devolve o valor logico verdade se a posicao estiver preenchida
;; e falso caso contrario 
(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
	(not (eq (logand (aref (tr-tab tabuleiro) linha) (aref *tabuleiro-mascaras* coluna)) 0))
)

;; tabuleiro-calcula-altura: tabuleiro x coluna x topo --> inteiro
;; esta funcao calcula a altura correspondente a coluna recebida
;; apartir de um topo recebido tambem por parametro
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

;; tabuleiro-altura-coluna: tabuleiro x coluna --> inteiro
;; este selector recebe um tabuleiro e um inteiro correspondente ao numero de uma coluna e
;; devolve a altura de uma coluna ou seja a posicao mais alta que esteja preenchida dessa coluna  
(defun tabuleiro-altura-coluna (tabuleiro coluna)
	(aref (tr-alturas tabuleiro) coluna)
)


(defun tabuleiro-linha-p (tabuleiro linha)
	(aref (tr-tab tabuleiro) linha))

;; tabuleiro-altura! tabuleiro x inteiro x inteiro --> tabuleiro
;; este modificador recebe um tabuleiro um inteiro correspondente ao numero da coluna e um inteiro
;; correspondente a altura e altera a altura no array alturas na coluna correspondente
(defun tabuleiro-altura! (tabuleiro coluna altura)
	(setf (aref (tr-alturas tabuleiro) coluna) altura)
T)



;; tabuleiro-altura-agregada: tabuleiro --> inteiro
;; calcula a soma das alturas todas para usar nas procuras informadas como heuristica
(defun tabuleiro-altura-agregada (tabuleiro)
	(reduce #' + (tr-alturas tabuleiro))
)

(defun list-of-bits (integer)
  (let ((bits '()))
    (dotimes (index (integer-length integer) bits)
      (push (if (logbitp index integer) 1 0) bits))))

(defun tabuleiro-espacos-livres (tabuleiro)
	(let (
		(espacos 0)
		)

		(loop for linha downfrom (1- *dim-linhas*) downto 0 do
 			(when (null (tabuleiro-linha-completa-p tabuleiro linha))
				(incf espacos (reduce #'+ (list-of-bits (tabuleiro-linha-p tabuleiro linha))))
			)
		)
	espacos)
)

;; tabuleiro-bumpiness: tabuleiro --> inteiro
;; a bumpiness de um tabuleiro diz-nos a variacao das alturas das coluna
;; e calculada atraves da soma absoluta entre as difrencas das colunas adjacentes
(defun tabuleiro-bumpiness (tabuleiro)
	(let(
		(total 0)
		)
		(loop for c from 0 below (1- *dim-colunas*) do
			(setf total 
				(abs 
					(+ total 
						(-
							(tabuleiro-altura-coluna tabuleiro c)
							(tabuleiro-altura-coluna tabuleiro (1+ c))
						)
					)
				)
			)
		)	
		(* 1 total))	
)


;; tabuleiro-linhas-completas: tabuleiro --> inteiro
;; calcula o numero de linhas completas em um tabuleiro
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

;; tabuleiro-linha-completa-p: tabuleiro x linha --> logico
;; este reconhecedor recebe um tabuleiro, um inteiro correspondente ao numero de uma linha
;; e devolve o valor logico verdade se todas as posicoes da linha recebida estiverem preenchidas
;; e falso caso contrario
(defun tabuleiro-linha-completa-p (tabuleiro linha) ; pode ser optimizado
	(eq (tabuleiro-linha-p tabuleiro linha) 1023)
)



;; tabuleiro-preenche!: tabuleiro x linha x coluna --> inteiro
;; este modificador recebe um tabuleiro um inteiro correspondente ao numero linha e um inteiro
;; correspondente ao numero da coluna e altera o tabuleiro recebido para na posicao correspondente
;; a linha e coluna passar a estar preenchido
(defun tabuleiro-preenche! (tabuleiro linha coluna)
	(when (and (<= 0 linha (1- *dim-linhas*)) (<= 0 coluna (1- *dim-colunas*)))
		(setf (aref (tr-tab tabuleiro) linha) (logior (aref (tr-tab tabuleiro) linha) (aref *tabuleiro-mascaras* coluna)))
		(when (> (1+ linha) (tabuleiro-altura-coluna tabuleiro coluna))
			(tabuleiro-altura! tabuleiro coluna (1+ linha))
		)
	)
T)

;; tabuleiro-muda-ponto!: tabuleiro x linha x coluna x valor
;; este modificador recebe um tabuleiro um inteiro correspondente ao numero linha, um inteiro
;; correspondente ao numero da coluna e o valor a preencher e altera o tabuleiro recebido para 
;; na posicao correspondente a linha e coluna passar a estar preenchido com o valor que foi escolhido
(defun tabuleiro-muda-ponto! (tabuleiro linha coluna valor) 
	(if valor
		(tabuleiro-preenche! tabuleiro linha coluna)
		(tabuleiro-apaga! tabuleiro linha coluna)
	)
T)

;; tabuleiro-apaga!: tabuleiro x linha x coluna --> inteiro
;; este modificador recebe um tabuleiro um inteiro correspondente ao numero linha e um inteiro
;; correspondente ao numero da coluna e altera o tabuleiro recebido para na posicao correspondente
;; a linha e coluna passar a estar apagada
(defun tabuleiro-apaga! (tabuleiro linha coluna)
	(when (and (<= 0 linha (1- *dim-linhas*)) (<= 0 coluna (1- *dim-colunas*)))
		(setf (aref (tr-tab tabuleiro) linha) (logand (aref (tr-tab tabuleiro) linha) (lognot (aref *tabuleiro-mascaras* coluna)))) 
		(when (<= linha (tabuleiro-altura-coluna tabuleiro coluna))			
			(tabuleiro-altura! tabuleiro coluna (tabuleiro-calcula-altura tabuleiro coluna linha))
		)
	)
T)


;; tabuleiro-desce-peca: tabuleiro x peca x coluna --> inteiro
;; esta funcao calcula a altura ate onde a peca pode descer ate encontrar o chao
;; ou um bloco preenchido onde pousar
(defun tabuleiro-desce-peca (tabuleiro peca coluna)
	(let (
		(drop (- *dim-linhas* (peca-dimensao-altura peca)))
	)
	(loop for l downfrom drop downto 0 do
		(when (null (tabuleiro-peca-pode-descer-p tabuleiro peca l coluna))
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
	
		(when (>= (+ linha dim-linhas-peca) *dim-linhas*)
			(return-from tabuleiro-peca-pode-descer-p T)
		)	
	
		(loop for pl downfrom (1- dim-linhas-peca) downto 0 do
			(loop for pc from 0 below dim-colunas-peca do
				(setf _pl (+ linha pl 1))
				(setf _pc (+ coluna pc))
				(when (and (eq (aref peca pl pc) T) (>= _pl 0))
					(when (not (and (< _pl *dim-linhas*) (null (tabuleiro-preenchido-p tabuleiro (1- _pl) _pc))))
						(return-from tabuleiro-peca-pode-descer-p NIL)
					)
				)		
			)
		)
		(return-from tabuleiro-peca-pode-descer-p T)
	)
)

;; tabuleiro-buracos: tabuleiro --> inteiro
;; esta funcao recebe um tabuleiro e devolve um inteiro correspondente ao numero de buracos no tabuleiro
;; um buraco e definido se houver um espaco vazio e houver pelo menos um bloco na mesma coluna preenchido
;; #S(TABULEIRO :TAB #(560 608 512 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) :ALTURAS #(3 0 0 2 2 1 0 0 0 0))
;; 9 buracos
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
				(eq (tabuleiro-linha-p tabuleiro min-y) 0)
			) do 
			(incf min-y)
		)
		
		(loop for y from min-y below *dim-linhas* do
			(setf line (tabuleiro-linha-p tabuleiro y))
			(setf filled (logand (lognot line) *grid-mask*))
			(setf under-mask (logior under-mask filled))
			(setf l-neighbor-mask (logior l-neighbor-mask (ash filled 1)))
			(setf r-neighbor-mask (logior r-neighbor-mask (ash filled -1)))
			(incf found-holes 
				(+
					(logcount (logand under-mask line))
					(logcount (logand l-neighbor-mask line))
					(logcount (logand r-neighbor-mask line))
				)
			)
		)
	found-holes)
)
 
;; tabuleiro-remove-linha!: tabuleiro x inteiro --> {}
;; este modificador recebe um tabuleiro, um inteiro correspondente ao numero linha, e altera
;; o tabuleiro recebido removendo essa linha do tabuleiro, e fazendo com que as linhas por cima
;; da linha removida descam uma linha.
(defun tabuleiro-remove-linha! (tabuleiro linha) ; pode ser optimizado, removendo varias
	(loop for i from linha below (1- *dim-linhas*) do
		(loop for j from 0 below *dim-colunas* do
			(tabuleiro-muda-ponto! tabuleiro i j (tabuleiro-preenchido-p tabuleiro (1+ i) j))
		)
	)
T)

;; tabuleiro-topo-preenchido-p: tabuleiro --> logico
;; este reconhecedor recebe um tabuleiro, e devolve o valor logico verdade se existir alguma posicao na linha do topo
;; do tabuleiro, retorna falso caso contrario
(defun tabuleiro-topo-preenchido-p (tabuleiro)
	(> (tabuleiro-linha-p tabuleiro (1- *dim-linhas*)) 0)
)

;; tabuleiro-iguais-p: tabuleiro x tabuleiro1 --> logico
;; este reconhecedor recebe dois tabuleiros, e devolve o valor logico verdade se os dois tabuleiros forem iguais e falso
;; caso contrario
(defun tabuleiros-iguais-p (tabuleiro tabuleiro1)
	(equalp tabuleiro tabuleiro1)	
)

(defun tabuleiro-vazio-p (tabuleiro)
	(loop for linha from 0 below (1- *dim-linhas*) do
		(when (> (tabuleiro-linha-p tabuleiro linha) 0)
			(return-from tabuleiro-vazio-p NIL)
		) 
	)
T)

;; array->tabuleiro: array --> tabuleiro
;; este transformador de saida recebe um array e controi um novo tabuleiro com o conteudo do array recebido e
;; devolve um tabuleiro
;; infelizmente devia a uma falha nos testes feitos pela cadeira e necessario verificar o rank de array recebido
;; devido ao nosso array tabuleiro ter apenas uma dimensao 
(defun array->tabuleiro (array)
	(let (
		(new-tabuleiro (cria-tabuleiro))
		(rank (array-rank array))
		(col 0)
	)

    (loop for i from 0 below *dim-linhas* do
		(when (> rank 1)
			(loop for j from 0 below *dim-colunas* do
					(when (aref array i j)
						(tabuleiro-preenche! new-tabuleiro i j)		
					)	
			)
		)
		(when (eq rank 1)
			(setf col 0)
			(when (> (aref array i) 0)
				(loop for bit in (list-of-bits (aref array i)) do
					(when (eq bit 1)
						(tabuleiro-preenche! new-tabuleiro i col)
					)
					(incf col)		
				)
			)
		)
	)
	new-tabuleiro)
)

;; tabuleiro->array: tabuleiro --> array
;; este transformador de saida recebe um tabuleiro e devolve um novo array igual ao array do tabuleiro recebido
(defun tabuleiro->array (tabuleiro)
	(let (
		(new-array 
			(make-array (list *dim-linhas* *dim-colunas*))
		)
	)

	(loop for i from 0 below *dim-linhas* do
		(loop for j from 0 below *dim-colunas* do
			(if (tabuleiro-preenchido-p tabuleiro i j)
				(setf (aref new-array i j) T)
			)		
		)
	)
	new-array)	
)

;;;;;;;;;;;;;;;;;;
;;  Tipo Estado ;;
;;;;;;;;;;;;;;;;;;

;; copia-estado: estado --> estado
;; este construtor recebe um estado e devolve um novo estado cujo conteudo deve ser copiado a partir do estado original.
(defun copia-estado (estado)
	(make-estado 
		:pontos (estado-pontos estado) 
		:pecas-por-colocar (copy-seq (estado-pecas-por-colocar estado)) 
		:pecas-colocadas (copy-seq (estado-pecas-colocadas estado)) 
		:tabuleiro (copia-tabuleiro (estado-tabuleiro estado))
	)
)

;; estado-accao: estado x accao x inteiro --> {}
;; este modificador recebe um estado, uma accao e um inteiro correspondente a linha em que a peca vai cair
;; e coloca a peca no tabuleiro
(defun estado-accao! (estado accao linha)
	(let (
		(peca-l (peca-dimensao-largura (accao-peca accao)))
		(peca-a (peca-dimensao-altura (accao-peca accao)))
		)
		(loop for i downfrom (1- peca-a) downto 0  do
			(loop for j from 0 below peca-l do
				(when (peca-preenchido-p (accao-peca accao) i j) 
					(tabuleiro-muda-ponto! (estado-tabuleiro estado) (+ linha i) (+ j (accao-coluna accao)) T)
				)
			)
		)
	)	
)

;; estados-iguais-p: estado x estado1 --> logico
;; este reconhecedor recebe dois estados e devolve o valor logico verdade se os dois estados forem iguais e falso
;; caso contrario
(defun estados-iguais-p (estado estado1)
	(and 
		(tabuleiros-iguais-p (estado-tabuleiro estado) (estado-tabuleiro estado1))
		(eq (estado-pontos estado) (estado-pontos estado1))
		(equal (estado-pecas-colocadas estado) (estado-pecas-colocadas estado1))
		(equal (estado-pecas-por-colocar estado) (estado-pecas-por-colocar estado1))	
	)
)

;; estado-final-p: estado --> logico
;; este reconhecedor recebe um estado e devolve o valor logico verdade se corresponder a um estado final onde o jogador
;; ja nao possa fazer mais jogadas e falso caso contrario. Um estado e considerado final se o tabuleiro tiver atingido o topo
;; ou se ja nao existe pecas por colocar
(defun estado-final-p (estado)
	(or (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)) (null (estado-pecas-por-colocar estado)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Funcoes do Problema de Procura ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; solucao: estado --> logico
;; esta funcao recebe um estado, e devolve o valor logico verdade se o estado recebido corresponder a uma solucao, e falso
;; caso contrario
(defun solucao (estado)
	(and (null (tabuleiro-topo-preenchido-p (estado-tabuleiro estado))) (null (estado-pecas-por-colocar estado)))
)

;; accoes: estado --> lista de accoes
;; esta funcao recebe um estado e devolve uma lista de accoes correspondendo a todas as accoes validas que podem ser feitas
;; com a proxima peca a ser colocada.
(defun accoes (estado)
	(let (
		(lista-accoes NIL)
		)
		(loop for peca in (pecas (car (estado-pecas-por-colocar estado))) do	
			(setf lista-accoes (peca-tabuleiro-accoes peca (estado-tabuleiro estado) lista-accoes))
		)
		(if lista-accoes		
			(return-from accoes (reverse lista-accoes))
		)
	)
)

;; resultado: estado x accao --> estado
;; esta funcao recebe um estado e devolve um novo estado que resulta de aplicar a accao recebida no estado original. 
;; depois de colocada a peca e verificado se o topo do tabuleiro esta preenchido se estiver nao se removem linhas se
;; nao estiver removem-se as linhas e calculam-se os pontos obtidos
(defun resultado (estado accao)
	(let (
		(new-estado NIL)
		(linhas-removidas 0)
		(altura (tabuleiro-altura-coluna (estado-tabuleiro estado) (accao-coluna accao)))
		)
		(setf new-estado (copia-estado estado))
		(if (>= (+ (1- altura) (peca-dimensao-altura (accao-peca accao))) *dim-linhas*)
			 (estado-accao! new-estado accao altura)
			(estado-accao! new-estado accao (tabuleiro-desce-peca (estado-tabuleiro new-estado) (accao-peca accao) (accao-coluna accao)))

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

;; qualidade: estado --> inteiro
;; estado funcao recebe um estado e retorna um valor de qualidade negativo que corresponde ao valor negativo dos
;; pontos ganhos ate ao momento
(defun qualidade (estado)
	(* -1 (estado-pontos estado))
)

;; custo-oportunidade: estado --> inteiro
;; esta funcao dado um estado devolve o custo de oportunidade de todas as accoes realizadas ate ao momento assumindo que e sempre
;; possivel fazer o maximo de pontos por cada peca colocada.
(defun custo-oportunidade (estado)
	(* 1 (-  
	 	(pecas-pontos-maximo (estado-pecas-colocadas estado))
	 	(estado-pontos estado)
	))
)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Tipo Node ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; cria-node-inicial: problema x heuristica (opcional) --> node
;; este constructor cria o node inicial de uma procura e aplica o valor do ser peso inicial
;; e devolve o node correspondente
(defun cria-node-inicial (problema &optional (heuristica (lambda (a) (declare (ignore a)) 0)))
	(make-node 
		:estado-actual (problema-estado-inicial problema)
		:pai NIL
		:profundidade 0
		:accao NIL
		:peso 
		(+ 
			(funcall (problema-custo-caminho problema) (problema-estado-inicial problema)) 
			(funcall heuristica (problema-estado-inicial problema))
		)
	)
)

;; cria-node-filho: problema pai accao heuristica (opcional) --> node
;; este constructor cria um node filho executa a funcao resultado do problema 
;; e atribui os valores correspondentes a profundidade, accao aplicada, peso e estado-actual
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

;; nodes-iguais-p: node1 node2 --> logico
;; este reconhecedor recebe dois nodes e verifica se eles sao iguais retornando um valor logico correspondente
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

;; buracos: estado --> inteiro
;; heuristica correspondente ao numero de buracos (objectivo minimiza-lo)
(defun buracos (estado)
	(tabuleiro-buracos (estado-tabuleiro estado))
)

;; media-alturas: estado --> float
;; heuristica correspondente a media das alturas totais (objectivo minimizar)
(defun media-alturas (estado)
	(/
		(altura-agregada estado)
		*dim-colunas*
	)
)

;; media-alturas: estado --> float
;; heuristica correspondente a media das alturas totais (objectivo minimizar)
(defun max-alturas (estado)
	(reduce #'max (tr-alturas (estado-tabuleiro estado)))
 )

;; linhas-completas: estado --> inteiro
;; heuristica correspondente ao numero de linhas completas no tabuleiro (objectivo minimizar)
;; (objectivo minimizar)
(defun linhas-completas (estado)
	(tabuleiro-linhas-completas (estado-tabuleiro estado))
)

;; altura-agregada: estado --> inteiro
;; heuristica correspondente a soma das alturas todas para usar nas procuras informadas como heuristica
;; (objectivo minimizar)
(defun altura-agregada (estado)
	(tabuleiro-altura-agregada (estado-tabuleiro estado))
)

;; bumpiness: estado --> inteiro
;; heuristica diz-nos a variacao das alturas das colunas
;; 'e calculada atraves da soma absoluta entre as difrencas das colunas adjacentes
(defun bumpiness (estado)
	(tabuleiro-bumpiness (estado-tabuleiro estado))
)

;; custo-ultimas-pecas: estado --> inteiro
;; calcula o desperdicio de pontos da ultima peca jogada no tabuleiro
(defun custo-ultimas-pecas (estado)
	(* 1 (-  
	 	(pecas-pontos-maximo-limitado (estado-pecas-colocadas estado))
	 	(estado-pontos estado)
	))
)

(defun espacos-livres (estado)
	(tabuleiro-espacos-livres (estado-tabuleiro estado))
)

;; heuristicas: estado --> inteiro
;; funcao que recebe um estado e retorna um inteiro correspondente a soma de todas as heuristicas
;; ou seja o valor correspondente a f(node)
(defun heuristicas(estado)
	;(declare (ignore estado))
	(+ 
		;(linhas-completas estado)
		;(custo-oportunidade3 estado)
		;(* 4 (max-alturas estado))
		(* 2.11 (altura-agregada estado))
		(* 3.4 (bumpiness estado))
		;(* 5.5 (espacos-livres estado))
		;(* 1 (linhas-transicoes estado))
		;(alturas-zero estado)
		;(media-alturas estado)
		(* 2.38 (qualidade estado))
		(* 20 (buracos estado))
	)
)

(defun bool-converter (flag)
	(if flag 1 0)
)

;; problema: tabuleiro x lista x funcao --> problema
;; constructor do tipo problema recebe um tabuleiro, as pecas por colocar e um funcao de custo
;; devolve o problema em questao para ser usado nas funcoes de procura
(defun formulacao-problema (tabuleiro pecas-por-colocar custo)
	(make-problema 
			:estado-inicial (make-estado :tabuleiro tabuleiro :pecas-por-colocar pecas-por-colocar)
			:solucao #'solucao
			:accoes #'accoes
			:resultado #'resultado
			:custo-caminho custo)
)

;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Procuras ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; procura-pp: problema -> lista accoes
;; esta funcao recebe um problema e usa a procura em profundidade primeiro em arvore
;; devolve um lista de accoes que se executado pela ordem certa leva a um estado objectivo
;; complexidade: 
;; tempo -> O(b^m)
;; espaco -> O(b*m)
(defun procura-pp (problema)
	(let (
		(solucao 
			(depth-first-search 
				problema
				(cria-node-inicial problema)
			)
		))
		(procura-get-solucao solucao))
)

;; depth-first-search: problema x node -> node
;; funcao auxiliar da procura-pp procura o node goal e constroi a solucao para depois
;; fazer backtrace da solucao na funcao procura-pp
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

;; procura-pp: problema x heuristica -> lista accoes
;; esta funcao recebe um problema e uma funcao heuristica e usa a procura A* em arvore e
;; devolve um lista de accoes que se executado pela ordem certa leva a um estado objectivo
;; complexidade: 
;; tempo -> Exponencial
;; espaco -> Exponencial
(defun procura-A* (problema heuristica)
	(let (
		(solucao (executa-procura 
			#'ida-star-search
			problema 
			heuristica)))
		(procura-get-solucao solucao))
)

;; a-star-search: problema x node x heuristica -> node
;; funcao auxiliar da procura-A* procura o node goal e constroi a solucao para depois
;; fazer backtrace da solucao na funcao procura-A*
(defun a-star-search (problema node heuristica)

	(let ((open (make-instance 'binary-heap))
		(current NIL)
		(accoes NIL)
		(new-node NIL)
		)
		(insert_heap open (node-peso node) node)
		(loop while (not (empty-p open)) do
			(setf current (extract-min open))
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
		)
	)
	NIL
)

;; get-hash-accoes: hash-table x estado --> lista accoes
;; hash-table para guardar as accoes pre calculadas das pecas
;; so e usada na procura best pois nas outras iria dar problemas nos testes
;; pois o objectivo e que elas funcionem em todos tipos de problemas
(defun get-hash-accoes (hash-table estado)
	(gethash (car (estado-pecas-por-colocar estado)) hash-table) 
)

;; procura-best: array x pecas-por-colocar --> lista accoes
(defun procura-best (array pecas-por-colocar)
	(let* (
		(tabuleiro (array->tabuleiro array))
		;(problema (formulacao-problema tabuleiro pecas-por-colocar #' (lambda (x) (declare (ignore x))0)))
		;(problema (formulacao-problema tabuleiro pecas-por-colocar #'qualidade))
		(problema (formulacao-problema tabuleiro pecas-por-colocar #'custo-oportunidade))
		;(problema (formulacao-problema tabuleiro pecas-por-colocar #'custo-ultimas-pecas))
		(solucao NIL))
		(loop for peca in (list 'i 'l 'j 'o 's 'z 't) do
			(setf (gethash peca *hash-accoes*) (accoes (make-estado :pontos 0 :pecas-por-colocar (list peca) :pecas-colocadas '() :tabuleiro (cria-tabuleiro))))
		)
		(cond 
			((not (tabuleiro-vazio-p tabuleiro))
				(setf solucao (executa-procura #'best-first-search problema #'heuristicas)))
				;(setf solucao (executa-procura #'best-first-search problema #'heuristicas *hash-accoes*)))
			(t 
				(setf problema (formulacao-problema tabuleiro pecas-por-colocar #'custo-ultimas-pecas))
				(setf solucao (executa-procura #'best-first-search problema #'heuristicas)))
		)
		(procura-get-solucao solucao)
	)
)

;; best-first-search: problema x node x heuristica -> node
;; funcao auxiliar da procura-best procura o node goal e constroi a solucao para depois
;; fazer backtrace da solucao na funcao procura-best
(defun best-first-search (problema node heuristica)
	(let ((open (make-instance 'binary-heap))
		(current NIL)
		(accoes NIL)
		(new-node NIL)
		)
		
		(insert_heap open (node-peso node) node)
		(loop while (not (empty-p open)) do
			(setf current (extract-min open))
			(when (funcall (problema-solucao problema) (node-estado-actual current))
				(return-from best-first-search current)
			)
			(if (estado-final-p (node-estado-actual current))
				(setf accoes NIL)
				(setf accoes (get-hash-accoes *hash-accoes* (node-estado-actual current)))
			)
			;(setf accoes (funcall (problema-accoes problema) (node-estado-actual current)))
			(loop for accao in accoes do
				(block continue
					(setf new-node (cria-node-filho problema current accao heuristica))
					(insert_heap open (node-peso new-node) new-node)
				)	
			)
		)
	)
	NIL
)

(defun procura-RBFS (problema heuristica)
	(let (
		(solucao (executa-procura 
			#'recursive-bfs
			problema 
			heuristica)))
		(procura-get-solucao solucao))
)

(defun procura-IDA* (problema heuristica)
	(let (
		(solucao (executa-procura 
			#'ida-star-search
			problema 
			heuristica)))
		(procura-get-solucao solucao))
)

;; recursive-best-first-search: problema x node x heuristica x bound --> node
;; implementamos este algoritmo mas nao utilizamos na nossa funcao best e mais lento
;; que o proprio A-star mas poupa imensa memoria vai ser usado para testar testes
;; para o relatorio
(defun recursive-bfs (problema node heuristica)
	(let (
		(result NIL)
		(best_f 0))
		(defun rbfs (problema node bound)
			(let 
				(
				(open (make-instance 'binary-heap))
				(accoes (funcall (problema-accoes problema) (node-estado-actual node)))
				(best NIL)
				(new-node NIL)
				(alternative NIL)
				(result NIL)
				(best_f 0)
				)
				
				(when (funcall (problema-solucao problema) (node-estado-actual node))
					(return-from rbfs (values node 0)))

				(when (null accoes)
					(return-from rbfs (values NIL MOST-POSITIVE-FIXNUM)))		

				(loop for accao in accoes do 
					(setf new-node (cria-node-filho problema node accao heuristica))
					; (if (< (node-peso node) (node-peso new-node))
					;  	(setf (node-peso node) (max (node-peso node) (node-peso new-node)))
					;  	(setf (node-peso node) (node-peso new-node))
					; )
					;(setf (node-peso new-node) (max (node-peso node) (node-peso new-node)))
					(insert_heap open (node-peso new-node) new-node)
				)

				(loop while T do
					(setf best (extract-min open))
					(when (> (node-peso best) bound) 
						(return-from rbfs (values NIL (node-peso best))))
					(if (> (heap-size open) 1)
						(setf alternative (node-peso (peek-min open)))
						(setf alternative MOST-POSITIVE-FIXNUM)
					)
					(multiple-value-setq (result best_f) (rbfs problema best (min bound alternative)))
					(when result
						(return-from rbfs (values result best_f)))
					(setf (node-peso best) best_f)
					(insert_heap open best_f best)
				)
			)
		)
	(multiple-value-setq (result best_f) (rbfs problema node MOST-POSITIVE-FIXNUM))
	result)
)

;; search-ida: problema x node x bound heuristica --> node
;; implementamos este algoritmo mas nao utilizamos na nossa funcao best e mais lento
;; que o proprio A-star mas poupa imensa memoria vai ser usado para testar testes
;; para o relatorio
(defun search-ida (problema node bound heuristica)
	(let (
		(f (node-peso node))
		(min MOST-POSITIVE-FIXNUM)
		(accoes NIL)
		(new-node NIL)
		(fn 0)
		(n NIL)
		)
		(when (> f bound)
			(return-from search-ida (values NIL f)))
		(when (funcall (problema-solucao problema) (node-estado-actual node))
			(return-from search-ida (values node f)))
		(setf accoes (reverse (funcall (problema-accoes problema) (node-estado-actual node))))
		(loop for accao in accoes do
			(setf new-node (cria-node-filho problema node accao heuristica))
			(multiple-value-setq (n fn) (search-ida problema new-node bound heuristica))
			(if n
				(return-from search-ida (values n fn))
				(setf min (min min fn))
			)
		)
	(values NIL min))
)

;; ida-star-search: problema x node x heuristica --> lista de accoes
;; implementamos este algoritmo mas nao utilizamos na nossa funcao best e mais lento
;; que o proprio A-star mas poupa imensa memoria vai ser usado para testar testes
;; para o relatorio
(defun ida-star-search (problema node heuristica)
	(let (
		(bound (node-peso node))
		(solucao NIL)
		(fn 0)
		)

		(loop while t do
			(setf solucao (search-ida problema node bound heuristica))
			(multiple-value-setq (solucao fn) (search-ida problema node bound heuristica))
			(cond 
				(solucao 
					(return-from ida-star-search solucao))
				((eq fn MOST-POSITIVE-FIXNUM) 
					(return-from ida-star-search NIL))
				(t 
					(setf bound fn))
			)
			
		)	
	)
)




;; executa-procura: algoritmo x problema x heuristica (opcional) x b (resto) --> node
;; funcao auxiliar par facilitar a execucao de um algoritmo
(defun executa-procura (algoritmo problema &optional (h (lambda (a) (declare (ignore a)) 0)) &rest resto)
	(apply algoritmo
		problema
		(cria-node-inicial problema h)
		h
		resto
	)
)

;; procura-get-solucao: node --> lista accoes
;; recebe um node goal e faz o backtrace da solucao retornando a lista de accoes
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;Priority Queues;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;lista ordenada;;;;;;;;;;;;;;;;
;;; seleciona-melhor: lista -> node 
;;; devolve o primeiro no da lista que corresponde ao no com o menor peso... 
(defun seleciona-melhor (lista)
	(car lista)
)

;;; Insere um no na lista ordenado por no-peso
;;; insere-lista: lista x node --> {}
;; complexidade: O(n)
(defun insere-lista (lista node)
	(cond ((null lista)(list node))
		((<= (node-peso node) (node-peso (car lista)))
			(cons node lista))
		(t 
			(cons (car lista) (insere-lista (cdr lista) node)))
	)
)	

;;;;;;;;;;;;;binary min-heap;;;;;;;;;;;;;;;; 

(deftype array-index () `(integer 0 ,(1- array-dimension-limit)))
(defconstant +initial-size+ 50 "initial queue vector size")


;; node binary heap
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

;; parent: key --> inteiro
;; devolve a key imutavel coorespondente ao pai
(defun parent (k)
  (declare (type array-index k))
  (floor (1- k) 2))

;; parent: key --> inteiro
;; devolve a key imutavel coorespondente ao filho da esquerda
(defun left (k)
  (declare (type (integer 0 #.(floor array-dimension-limit 2)) k))
  (1+ (* k 2)))

;; parent: key --> inteiro
;; devolve a key imutavel coorespondente ao filho da direita
(defun right (k)
  (declare (type (integer 0 #.(floor array-dimension-limit 2)) k))
  (* (1+ k) 2))

;; peek-min: heap --> node
;; recebe um heap e devolve o node com o peso mais pequeno
(defun peek-min (heap)
  (let ((node (aref (bin-heap-array heap) 0)))
    (values (node_bh-data node)
            (node_bh-key node))))

;; peek-min: heap --> bh-node
;; recebe um heap e devolve o binary heap node com o peso mais pequeno
(defun peek-min-node (heap)
  (let ((node (aref (bin-heap-array heap) 0)))
    node))

;; heap-contains: heap x node --> logico
;; verifica se este node esta contido no heap
(defun heap-contains (heap node)
	(if (< (node_bh-index node) (heap-size heap))
		(equalp 
			(aref (bin-heap-array heap) (node_bh-index node))
			node
		)
		NIL
	)
)

;; clear-heap: heap --> {}
;; apaga todos os nodes do min-heap
(defun clear-heap (heap)
  (setf (fill-pointer (bin-heap-array heap)) 0))

;; empty-p: heap --> logico
;; verifica se um heap esta vazio ou nao
(defun empty-p (heap)
  (zerop (fill-pointer (bin-heap-array heap))))

;; heap-size: heap --> inteiro
;; recebe um heap e retorna um valor inteiro correspondente ao numero de nodes inseridos
(defun heap-size (heap)
  (length (bin-heap-array heap)))

;; extract-min: heap --> node
;; recebe um heap e retorna o node com o peso mais pequeno no heap e reajusta os nodes
;; complexidade: O(log(n))
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

;; swap-nodes: array x inteiro x inteiro --> {}
;; troca o node no indice i pelo node do indice j fazendo a rotacao necessaria
(defun swap-nodes (array i j)
  (declare (type array-index i j))
  (setf (node_bh-index (aref array i)) j
        (node_bh-index (aref array j)) i)
  (rotatef (aref array i) (aref array j)))

;; sink: array x index --> {}
;; recebe um array e o indice modificando o array reajustado-o
;; depois de um node ser removido e percorrida esta funcao para reajustar os nodes do heap
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

;; percolate-up: array index --> {}
;; recebe um array e um indice e modifica o array reajustando-o os nos
;; depois de um node ser adicionado e' percorrida esta funcao para reajustar os nodes do heap
(defun perlocate-up (array vindex)
  (loop for index = vindex then parent
        for parent = (parent index)
        with key = (node_bh-key (aref array vindex))
        while (and (>= parent 0)
                   (<= key (node_bh-key (aref array parent))))
        do (swap-nodes array index parent) 
        finally (return (aref array index))))

;; insert_heap: heap x peso x node --> {}
;; insere um no' num heap de forma ordenada, nao retorna nada
;; complexidade: O(log(n))
(defun insert_heap (heap key data)
  (let ((node (%make-node_bh key data 0))
        (array (bin-heap-array heap)))
    (perlocate-up array (setf (node_bh-index node) 
                              (vector-push-extend node array)))))
(load "utils.fas")