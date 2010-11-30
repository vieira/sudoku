;;;; Solucionador de Sudoku
;;;; 2010 - Projecto de IA

;;;; Definição da estrutura nó
(defstruct no tabuleiro)


;;;; Constructores
(defun faz-tabuleiro (tamanho valor)
  "Cria tabuleiro de dimensão 'tamanho' inicilizado a 'valor'
  faz-tabuleiro: inteiro x inteiro -> tabuleiro"
  (make-list tamanho :initial-element (make-list tamanho :initial-element valor)))


(defun tabuleiro-poe-numero (tabuleiro numero linha coluna)
  "Devolve um novo tabuleiro construído a partir da inserção
  de um 'numero' na 'linha' e 'coluna' de 'tabuleiro'
  tabuleiro-por-numero: tabuleiro x inteiro x inteiro x inteiro -> tabuleiro"
  (let ((novo-tabuleiro (copy-tree tabuleiro)))
    (and (setf (nth coluna (nth linha novo-tabuleiro)) numero)
         novo-tabuleiro)))

;;;; Selectores
(defun tabuleiro-numero (tabuleiro linha coluna)
  "Devolve o valor que se contra na posição dada por 'linha' e 'coluna'
  tabuleiro-numero: tabuleiro x inteiro x inteiro -> tabuleiro"
  (nth coluna (nth linha tabuleiro)))
	
(defun tabuleiro-dimensao (tabuleiro)
  "Retorna a dimensão do 'tabuleiro' recebido.
  tabuleiro-dimensao: tabuleiro -> inteiro"
	(length tabuleiro))
	

;;;; Reconhecedores
(defun tabuleiro-p (tabuleiro)
  "Recebe um elemento e verifica se é um tabuleiro válido.
  tabuleiro-p: universal -> booleano"
  (typecase tabuleiro
      (list (let ((tamanho (length tabuleiro)))
              (loop for linha in tabuleiro
                    always (and (typecase linha (list T))
                                (eq tamanho (length linha))))))))


;;;; Transformadores
(defun le-tabuleiro (ficheiro)
  "Faz parsing de um ficheiro de texto correspondente
  à representação de uma matriz e devolve o respectivo
  tabuleiro.
  le-tabuleiro: stream -> tabuleiro"
  (with-open-file (f ficheiro)
    (and (parse-integer (read-line f))
         (loop for line = (read-line f NIL NIL) 
               while line 
               collect 
               (read-from-string 
                 (concatenate 'string "(" line ")"))))))

    (defun escreve-tabuleiro (tabuleiro)
      "Recebe um tabuleiro e imprime as linhas que o compõe
      com os dígitos separados por um espaço.
  escreve-tabuleiro: tabuleiro -> NIL"
  (loop for linha in tabuleiro
        do (format t "~{~S~^ ~}~%" linha)))

	
;;;; Função Geral de Procura
(defun procura (ficheiro &optional (estrategia :informada))
  (cond ((eq estrategia :profundidade) 
         (no-tabuleiro 
           (procura-profundidade 
             (make-no :tabuleiro (le-tabuleiro ficheiro))
             #'objectivo 
             #'sucessores)))
        ((eq estrategia :largura)
         (no-tabuleiro 
           (procura-largura 
             (make-no :tabuleiro (le-tabuleiro ficheiro))
             #'objectivo 
             #'sucessores)))
        ((eq estrategia :retrocesso)
         (no-tabuleiro 
           (procura-profundidade 
             (make-no :tabuleiro (le-tabuleiro ficheiro))
             #'objectivo 
             #'sucessores)))
        ((eq estrategia :informada)
         (no-tabuleiro 
           (retrocesso-informada
             (make-no :tabuleiro (le-tabuleiro ficheiro))
             #'objectivo 
             #'sucessores)))
        (t (print "Estratégia indisponível"))))


;;;; Algoritmos genericos de procura em arvore
(defun procura-arvore (estados objectivo sucessores ordem) 
  "Procura, comecando em estados e de acordo com ordem e
  sucessores, um no que satisfaca a funcao objectivo."
  (cond ((funcall objectivo (first estados)) (first estados))
        (t (procura-arvore
             (funcall ordem
                      (funcall sucessores (first estados))
                      (rest estados))
             objectivo sucessores ordem))))

(defun procura-profundidade (inicial objectivo sucessores)
  "Procura na arvore de estados de tal forma que o ramo n
  sera percorrido depois de atingida as folhas do ramo n-1."
  (procura-arvore (list inicial) objectivo sucessores #'append))

(defun procura-largura (inicial objectivo sucessores)
  "Procura na arvore de estados de tal forma que todos os
  pais sao expandidos antes dos filhos serem testados."
  (procura-arvore (list inicial) objectivo sucessores #'prepend))
  
(defun prepend (a b) "Coloca b no inicio a" (append b a))

(defun retrocesso-informada (inicial objectivo sucessores)
  (procura-arvore (list inicial)
                  objectivo 
                  #'(lambda (no)
                      (funcall 
                        sucessores no :criterio #'posicao-mais-restringida))
                  #'append))


;;;; Funcoes de suporte a procura em arvore especificas do problema
(defun objectivo (estado)
  "Verifica se estado e o estado objectivo do jogo."
  (let ((tabuleiro-lista 
          (loop for linha in (no-tabuleiro estado) append linha)))
    (loop for valor in tabuleiro-lista 
          never (or (listp valor) (zerop valor)))))


(defun sucessores (actual &key (criterio #'first))
  "Gera uma lista de nos sucessores do no no actual dado, tendo em conta
  as regras do jogo e as possiveis proximas jogadas.
  sucessores: nó -> lista de nós"
  (let* ((tabuleiro (no-tabuleiro actual))
         (tamanho-tabuleiro (tabuleiro-dimensao tabuleiro))
         (posicao (posicao-vazia tabuleiro :criterio criterio))
         (linha (car posicao))
         (coluna (cdr posicao)))
    (loop for numero from 1 to tamanho-tabuleiro
          when (numero-valido-p tabuleiro numero linha coluna)
          collect (make-no :tabuleiro (tabuleiro-poe-numero 
                                        tabuleiro numero linha coluna)))))


(defun raiz (jogo)
  "Recebe a matriz relativa ao estado actual do jogo e devolve um no
  a ser expandido, quando um dos extremos foi alcancado."
  (make-no :tabuleiro jogo))


;;;; Funções específicas do Sudoku
(defun numero-valido-p (tabuleiro numero linha coluna)
  "Recebe um 'tabuleiro' e verifica se o 'numero' fornecido é uma jogada
  válida para a posição dada por 'linha' e 'coluna'.
  numero-valido-p: tabuleiro x inteiro x inteiro x inteiro -> booleano"
  ;; TODO(vieira@yubo.be): Optimizar isto, talvez recebendo tamanho-grupo?
  (let* ((tamanho-tabuleiro (tabuleiro-dimensao tabuleiro))
         (tamanho-grupo (floor (log tamanho-tabuleiro 2)))
         (l (* (floor (/ linha tamanho-grupo)) tamanho-grupo))
         (c (* (floor (/ coluna tamanho-grupo)) tamanho-grupo)))
    (loop for i from 0 below tamanho-tabuleiro do
          (let ((numero-linha (tabuleiro-numero tabuleiro linha i))
                (numero-coluna (tabuleiro-numero tabuleiro coluna i))
                (numero-caixa (tabuleiro-numero
                                tabuleiro
                                (+ l (mod i tamanho-grupo))
                                (+ c (floor (/ i tamanho-grupo))))))
          (and (or (if (integerp numero-linha) (= numero numero-linha)))
                   (if (integerp numero-coluna) (= numero numero-coluna))
                   (if (integerp numero-caixa) (= numero numero-caixa)))
               (return NIL))
          finally (return T))))


(defun posicao-vazia (tabuleiro &key (criterio #'first))
  "Função que recebe um 'tabuleiro' e um 'criterio' e devolve
  a posição vazia que cumpre o 'criterio' especificado. Se
  nenhum critério for fornecido devolve, por definição,
  a primeira posição encontrada que esteja vazia.
  posicao-vazia: tabuleiro x funcao -> par"
  (let ((tamanho-tabuleiro (tabuleiro-dimensao tabuleiro)))
    (if (eql criterio #'first)
      ;; No caso do critério ser simplesmente a primeira posição
      ;; vazia evita-se construir a lista de todas as posições
      ;; retornando logo que apareça a primeira.
      (loop for i from 0 below tamanho-tabuleiro do
            (let ((coluna (position 0 (nth i tabuleiro))))
              (if (not (null coluna))
                (return (cons i coluna)))))
      
      ;; No caso de ser fornecido um critério para a selecção da
      ;; posição vazia, executa-se a função fornecida com a lista
      ;; de todas as posições vazias como argumento.
      (funcall criterio tabuleiro
               (loop for l from 0 below tamanho-tabuleiro append 
                     (loop for c from 0 below tamanho-tabuleiro
                           when (zerop (tabuleiro-numero tabuleiro l c))
                           collect (cons l c)))))))


;;;; Heurística MRV (ou Most Constrained Variable)
(defun posicao-mais-restringida (tabuleiro posicoes)
  (loop for posicao in posicoes
        for restricoes = (length (restricoes tabuleiro 
                                             (car posicao) 
                                             (cdr posicao)))
        with mais-restringida = nil 
        with mais-restricoes = 0
        when (> restricoes mais-restricoes)
        do (and (setf mais-restricoes restricoes)
                (setf mais-restringida posicao))
        finally (return mais-restringida)))


(defun restricoes (tabuleiro linha coluna)
  (let* ((tamanho-tabuleiro (tabuleiro-dimensao tabuleiro))
         (tamanho-grupo (floor (log tamanho-tabuleiro 2)))
         (l (* (floor (/ linha tamanho-grupo)) tamanho-grupo))
         (c (* (floor (/ coluna tamanho-grupo)) tamanho-grupo)))
    (loop for i from 0 below tamanho-tabuleiro with restricoes = NIL do
          (let ((numero-linha (tabuleiro-numero tabuleiro linha i))
                (numero-coluna (tabuleiro-numero tabuleiro i coluna))
                (numero-caixa (tabuleiro-numero 
                                tabuleiro
                                (+ l (mod i tamanho-grupo))
                                (+ c (floor (/ i tamanho-grupo))))))
            (if (and (integerp numero-linha) (> numero-linha 0))
              (setf restricoes (cons numero-linha restricoes)))
            (if (and (integerp numero-coluna) (> numero-coluna 0))
              (setf restricoes (cons numero-coluna restricoes)))
            (if (and (integerp numero-caixa) (> numero-caixa 0))
              (setf restricoes (cons numero-caixa restricoes))))
            finally (return (remove-duplicates restricoes)))))


;;;; Propagação de Restrições
;;; TODO(vieira@yubo.be): !!!DESTRUCTIVO!!!
(defun atribui(tabuleiro objectivo)
  (let* ((tamanho-tabuleiro (tabuleiro-dimensao tabuleiro))
         (todos-numeros (loop for i from 1 to tamanho-tabuleiro collect i)))
    (loop for i from 0 to tamanho-tabuleiro
          do (loop for j from 0 to tamanho-tabuleiro
                   when 
                   (or (listp (tabuleiro-numero tabuleiro i j))
                       (zerop (tabuleiro-numero tabuleiro i j)))
                   do
                   (let* ((posicao (tabuleiro-numero tabuleiro i j))
                          (possiveis (cond ((integerp posicao)
                                            (set-difference
                                              todos-numeros
                                              (restricoes tabuleiro i j)))
                                           ((listp posicao) posicao))))
                     (cond ((zerop (length possiveis))
                            (return NIL))
                           ((= (length possiveis) 1)
                            (and (setf (nth j (nth i tabuleiro)) 
                                       (car possiveis))
                                 (atribui tabuleiro objectivo)))
                           (t (loop for numero in possiveis
                                    if (not (numero-valido-p 
                                              tabuleiro 
                                              numero 
                                              i 
                                              j))
                                    do (delete numero possiveis))))))
          finally (return tabuleiro))))
