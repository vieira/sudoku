;;;; Solucionador de Hidato
;;;; João Vieira e Ricardo Pateiro
;;;; 2009 - Projecto de IA
;;;;
;;;; Código elegante (convenhamos),
;;;; algorítmo ineficiente (concedo).
;;;; Ver Relatório.

;;;; Definição da estrutura nó
(defstruct no jogo posicao jogadas)

;;;; Função principal
(defun resolve-hidato (jogo)
  "Resolve o jogo hidato expandido as várias possibilidades válidas
  em cada jogada e fazendo uma procura em árvore pelo estado final
  que corresponde ao jogo resolvido."
  (no-jogo (procura-profundidade (raiz jogo) #'objectivo #'sucessores)))
;(procura-largura (raiz jogo) #'objectivo #'sucessores)) ; não testada!
;(procura-melhor-primeiro (raiz jogo) #'objectivo #'sucessores #'custo-fn))

;;;; Algorítmos genéricos de procura em árvore
(defun procura-arvore (estados objectivo sucessores ordem)
  "Procura, começando em estados e de acordo com ordem e
  sucessores, um nó que satisfaça a função objectivo."
  (cond ((funcall objectivo (first estados)) (first estados))
        (t (procura-arvore
             (funcall ordem
                      (funcall sucessores (first estados))
                      (rest estados))
             objectivo sucessores ordem))))

(defun procura-profundidade (inicial objectivo sucessores)
  "Procura na árvore de estados de tal forma que o ramo n
  será percorrido depois de atingida as folhas do ramo n-1."
  (procura-arvore (list inicial) objectivo sucessores #'append))

(defun procura-largura (inicial objectivo sucessores)
  "Procura na árvore de estados de tal forma que todos os
  pais são expandidos antes dos filhos serem testados."
  (procura-arvore (list inicial) objectivo sucessores #'prepend))

(defun procura-melhor-primeiro (inicial objectivo sucessores custo-fn)
  "Procura nos estados de menor custo primeiro até que um estado
  objectivo seja encontrado."
  (procura-arvore (list inicial) objectivo sucessores (ordem custo-fn)))

(defun ordem (custo-fn)
  "Gera uma função que ordena de acordo com custo-fn."
  #'(lambda (proximo actual)
      (sort (append proximo actual) #'< :key custo-fn)))

(defun prepend (a b) "Coloca b no inicio a" (append b a))

;;;; Funções de suporte à procura em árvore específicas do problema
(defun objectivo (estado)
  "Verifica se estado é o estado objectivo do jogo."
  (let ((jogo-lista (loop for linha in (no-jogo estado) append linha)))
    (if (= (length jogo-lista) (length (remove-duplicates jogo-lista)))
      (loop for valor in jogo-lista 
            never (zerop valor)))))

(defun sucessores (actual)
  "Gera uma lista de nós sucessores do no nó actual dado, tendo em conta
  as regras do jogo e as possíveis próximas jogadas."
  (let ((posicao-sucessor
          (loop for posicao in (posicoes-adjacentes (no-jogo actual) (no-posicao actual))
                when (= (valor (no-jogo actual) posicao) 
                        (1+ (valor (no-jogo actual) (no-posicao actual))))
                return posicao)))
    (cond ((not (null posicao-sucessor))
           (list (make-no
                   :jogo (no-jogo actual)
                   :posicao posicao-sucessor
                   :jogadas (no-jogadas actual))))
          (t (loop for posicao-candidato in (posicoes-adjacentes (no-jogo actual) (no-posicao actual))
                   if (= (valor (no-jogo actual) posicao-candidato) 0)
                   collect (make-no 
                             :jogo (insere-jogo (no-jogo actual)
                                                posicao-candidato
                                                (1+ (valor (no-jogo actual) (no-posicao actual))))
                             :posicao posicao-candidato
                             :jogadas (1- (jogadas (no-jogo actual)))))))))

(defun raiz (jogo)
  "Recebe a matriz relativa ao estado actual do jogo e devolve um nó
  a ser expandido, quando um dos extremos foi alcançado."
  (make-no :jogo jogo
           :posicao (maximo-contiguo jogo 1)
           :jogadas (jogadas jogo)))

(defun custo-fn (actual)
  "Faz uma estimativa educada e optimista do custo até a solução se
  o nó actual for seguido."
  (+ (estimativa actual) (no-jogadas actual)))

(defun estimativa (actual)
  "Devolve uma estimativa do melhor nó a seguir baseado na diferença
  de valores e respectiva distância no tabuleiro."
  (let ((ranking 2))
    (loop for posicao in (posicoes-adjacentes (no-jogo actual) (no-posicao actual))
          do (progn
               (if (= (valor (no-jogo actual) posicao) 
                      (1+ (valor (no-jogo actual) (no-posicao actual)))) (decf ranking))
               (if (= (valor (no-jogo actual) posicao) 
                      (1- (valor (no-jogo actual) (no-posicao actual)))) (decf ranking))))
    ranking))

;;;; Funções auxiliares genéricas para matrizes n x m
(defun valor (jogo posicao)
  "Devolve o valor de posicao na matriz jogo."
  (let ((linha (car posicao))
        (coluna (cadr posicao)))
    (nth coluna (nth linha jogo))))

(defun distancia (posicao-i posicao-ii)
  "Calcula a distância de Chebyshev entre posicao-ii e 
  posicao-i."
  (max (abs (- (car posicao-ii) (car posicao-i)))
       (abs (- (cadr posicao-ii) (cadr posicao-ii)))))

(defun insere-jogo (jogo posicao valor)
  "Insere numa posicao da matriz jogo o valor dado."
  (if (null posicao)
    valor
    (let* ((final (nthcdr (car posicao) jogo))
           (inicio (ldiff jogo final))
           (a-inserir (first final))
           (resto (rest final)))
      (append inicio (list (insere-jogo a-inserir (rest posicao) valor)) resto))))

(defun maximo (jogo)
  "Procura na matriz jogo o valor máximo e devolve a sua posição."
  (let ((jogo-lista (loop for linha in jogo append linha)))
    (multiple-value-list 
      (floor (position 
               (reduce #'max jogo-lista)
               jogo-lista)
             (length (car jogo))))))

(defun minimo (jogo)
  "Procura na matriz jogo o mínimo positivo e devolve a sua posição."
  (let ((jogo-lista (loop for linha in jogo append linha)))
    (multiple-value-list 
      (floor (position 
               (reduce #'min
                       (delete 0 jogo-lista)) jogo-lista)
             (length (car jogo))))))

;;;; Funções auxiliares na determinação de nó a expandir
(defun jogadas (jogo)
  "Calcula o número de jogadas até o final do jogo."
  (let ((jogo-lista (loop for linha in jogo append linha)))
    (count 0 jogo-lista)))

(defun seguinte (jogo posicao)
  "Devolve a posição do valor mais próximo ao valor de posicao na
  matriz jogo."
  (let ((valor-seguinte (valor jogo posicao))
        (jogo-lista (loop for linha in jogo append linha)))
    (loop while
          (null (position (incf valor-seguinte) jogo-lista)))
    (multiple-value-list
      (floor (position valor-seguinte jogo-lista) (length (car jogo))))))

(defun maximo-contiguo (jogo valor)
  "Devolve a posição correspondente ao valor máximo que é posível
  atingir através de incrementos unitários sucessivos partindo 
  de valor."
  (let ((maximo valor)
        (jogo-lista (loop for linha in jogo append linha)))
    (loop while 
          (position maximo jogo-lista)
          do (incf maximo)
          finally (decf maximo))
    (multiple-value-list
      (floor (position maximo jogo-lista) (length (car jogo))))))

(defun minimo-contiguo (jogo valor)
  "Devolve a posição correspondente ao valor mínimo positivo
  que é posível atingir através de decrementos unitários 
  sucessivos partindo de valor."
  (let ((minimo valor)
        (jogo-lista (loop for linha in jogo append linha)))
    (loop while 
          (and (position minimo jogo-lista) (plusp minimo))
          do (decf minimo)
          finally (incf minimo))
    (multiple-value-list
      (floor (position minimo jogo-lista) (length (car jogo))))))

(defun posicoes-adjacentes (jogo posicao)
  "Calcula uma lista de posições adjacentes a posicao
  na matriz jogo, tendo em conta as respectivas dimensões."
  (loop for (linha coluna) in
        (loop for (linha coluna) in
              (loop for x from -1 to 1 append
                    (loop for y from -1 to 1 when
                          (not (and (= x 0) (= y 0)))
                          collect (list x y)))
              collect (list (+ linha (car posicao)) (+ coluna (cadr posicao))))
        when (and 
               (< linha (length jogo))
               (< coluna (length (car jogo)))
               (not (minusp linha))
               (not (minusp coluna)))
        collect (list linha coluna)))

;;;; Métodos de Debugging
(defun activa-debug (&optional funcao)
  "Activa o trace para a funcao especificada ou
  para todas as funções do programa."
  (if (not (null funcao)) (trace funcao)
    (progn (trace resolve-hidato)
           (trace procura-arvore)
           (trace procura-profundidade)
           (trace procura-melhor-primeiro)
           (trace seguinte)
           (trace distancia)
           (trace estimativa)
           (trace ordem)
           (trace custo-fn)
           (trace objectivo)
           (trace sucessores)
           (trace raiz)
           (trace valor)
           (trace insere-jogo)
           (trace maximo)
           (trace minimo)
           (trace maximo-contiguo)
           (trace minimo-contiguo)
           (trace posicoes-adjacentes))))

;;;; Optimizações de Compilador
(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))
