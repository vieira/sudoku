;;;; Solucionador de Sudoku
;;;; 2010 - Projecto de IA

;;;; Definicao da estrutura no
(defstruct no tabuleiro)
(defstruct assignment tabuleiro posicao)


;;;; Constructores
(defun faz-tabuleiro (tamanho valor)
  "Cria tabuleiro de dimensao 'tamanho' inicilizado a 'valor'
  faz-tabuleiro: inteiro x inteiro -> tabuleiro"
  (if (listp valor)
    (loop for i from 0 below tamanho collect
          (loop for i from 0 below tamanho collect
                (copy-list valor)))
    (loop for i from 0 below tamanho collect
          (make-list tamanho :initial-element valor))))


(defun tabuleiro-poe-numero (tabuleiro numero linha coluna)
  "Devolve um novo tabuleiro construido a partir da insercao
  de um 'numero' na 'linha' e 'coluna' de 'tabuleiro'
  tabuleiro-por-numero: tabuleiro x inteiro x inteiro x inteiro -> tabuleiro"
  (let ((novo-tabuleiro (copy-tree tabuleiro)))
    (and (setf (nth coluna (nth linha novo-tabuleiro)) numero)
         novo-tabuleiro)))

;;;; Selectores
(defun tabuleiro-numero (tabuleiro linha coluna)
  "Devolve o valor que se contra na posicao dada por 'linha' e 'coluna'
  tabuleiro-numero: tabuleiro x inteiro x inteiro -> tabuleiro"
  (nth coluna (nth linha tabuleiro)))
	
(defun tabuleiro-dimensao (tabuleiro)
  "Retorna a dimensao do 'tabuleiro' recebido.
  tabuleiro-dimensao: tabuleiro -> inteiro"
	(length tabuleiro))
	

;;;; Reconhecedores
(defun tabuleiro-p (tabuleiro)
  "Recebe um elemento e verifica se e um tabuleiro valido.
  tabuleiro-p: universal -> booleano"
  (typecase tabuleiro
      (list (let ((tamanho (length tabuleiro)))
              (loop for linha in tabuleiro
                    always (and (typecase linha (list T))
                                (eq tamanho (length linha))))))))


;;;; Transformadores
(defun le-tabuleiro (ficheiro)
  "Faz parsing de um ficheiro de texto correspondente
  a representacao de uma matriz e devolve o respectivo
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
  "Recebe um tabuleiro e imprime as linhas que o compoe
  com os digitos separados por um espaco.
  escreve-tabuleiro: tabuleiro -> NIL"
  (format t "~d~%" (length tabuleiro))
  (loop for linha in tabuleiro
        do (format t "~{~S~^ ~}~%" linha)))

(defun no-para-tabuleiro (no)
  (let ((tabuleiro (no-tabuleiro no)))
    (loop for linha in tabuleiro collect
          (mapcar #'car linha))))

;;;; Funcao Geral de Procura
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
         (assignment-tabuleiro
           (retrocesso 
             (make-assignment :tabuleiro (le-tabuleiro ficheiro)
                              :posicao (posicao-vazia (le-tabuleiro ficheiro)))
             #'objectivo-retrocesso)))
        ((eq estrategia :informada)
         (retrocesso-informada
             (make-no :tabuleiro (le-tabuleiro ficheiro))
             #'objectivo-informada 
             #'sucessores-informada))
        (t (print "Estrategia desconhecida"))))


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

(defun retrocesso (no objectivo)
  "Passa uma referencia de tabuleiro e a posicao alterada, quando
  atinge um candidato parcial que viola alguma das restricoes
  reverte as alteracoes feitas ao tabuleiro passado como referencia
  e continua a procura"
  (let* ((tabuleiro (assignment-tabuleiro no))
         (tamanho-tabuleiro (tabuleiro-dimensao tabuleiro)))
    (cond ((funcall objectivo no) no)
          (t (let* ((proxima-posicao (posicao-vazia tabuleiro))
                    (linha (car proxima-posicao))
                    (coluna (cdr proxima-posicao)))
               (loop for i from 1 to tamanho-tabuleiro
                     when (numero-valido-p tabuleiro i linha coluna)
                     do (and (setf (nth coluna (nth linha tabuleiro)) i)
                             (let ((resultado
                                     (retrocesso (make-assignment 
                                                   :tabuleiro tabuleiro
                                                   :posicao proxima-posicao)
                                                 objectivo)))
                               (if resultado (return resultado))
                               (setf (nth coluna (nth linha tabuleiro))
                                     0)))))))))


(defun retrocesso-informada (inicial objectivo sucessores)
  (let ((raiz (make-no :tabuleiro (propaga (no-tabuleiro inicial)))))
    (no-para-tabuleiro
      (procura-arvore (list raiz)
                      objectivo
                      sucessores
                      #'append))))


;;;; Funcoes objectivo para os varios tipos de procura
(defun objectivo (estado)
  "Verifica se estado e o estado objectivo do jogo."
  (let ((tabuleiro (no-tabuleiro estado)))
    (loop for linha in tabuleiro 
          always (loop for valor in linha
                       never (zerop valor)))))

(defun objectivo-retrocesso (estado)
  "Verifica se estado e o estado objectivo do jogo."
  (let ((tabuleiro (assignment-tabuleiro estado)))
    (loop for linha in tabuleiro 
          always (loop for valor in linha
                       never (zerop valor)))))


(defun objectivo-informada (estado)
  "Verifica se estado e o estado objectivo do jogo."
  (let ((tabuleiro (no-tabuleiro estado)))
    (loop for linha in tabuleiro
          always (loop for valor in linha
                       always (= (length valor) 1)))))

;;;; Funcoes sucessores para os varios tipos de procura
(defun sucessores (actual)
  "Gera uma lista de nos sucessores do no no actual dado, tendo em conta
  as regras do jogo e as possiveis proximas jogadas.
  sucessores: no -> lista de nos"
  (let* ((tabuleiro (no-tabuleiro actual))
         (tamanho-tabuleiro (tabuleiro-dimensao tabuleiro))
         (posicao (posicao-vazia tabuleiro :criterio #'first))
         (linha (car posicao))
         (coluna (cdr posicao)))
      (loop for numero from 1 to tamanho-tabuleiro
            when (numero-valido-p tabuleiro numero linha coluna)
            collect (make-no :tabuleiro (tabuleiro-poe-numero 
                                          tabuleiro numero linha coluna)))))


(defun sucessores-informada (actual)
  "Gera uma lista de nos sucessores do no no actual dado, tendo em conta
  as regras do jogo e as possiveis proximas jogadas.
  sucessores: no -> lista de nos"
  (let* ((tabuleiro (no-tabuleiro actual))
         (posicao (posicao-vazia tabuleiro 
                                 :criterio #'posicao-mais-restringida))
         (linha (car posicao))
         (coluna (cdr posicao)))
    (loop for numero in (tabuleiro-numero tabuleiro linha coluna)
          with sucessores = NIL
          do (let ((sucessor (atribui (copy-tree tabuleiro)
                                      numero
                                      linha
                                      coluna)))
               (or (null sucessor)
                   (setf sucessores
                         (cons (make-no :tabuleiro sucessor) 
                               sucessores))))
          finally (return sucessores))))


;;;; Funcoes para validacao de solucoes parciais
(defun numero-valido-p (tabuleiro numero linha coluna)
  "Recebe um 'tabuleiro' e verifica se o 'numero' fornecido e uma jogada
  valida para a posicao dada por 'linha' e 'coluna'.
  numero-valido-p: tabuleiro x inteiro x inteiro x inteiro -> booleano"
  (let* ((tamanho-tabuleiro (tabuleiro-dimensao tabuleiro))
         (tamanho-grupo (floor (log tamanho-tabuleiro 2)))
         (l (* (floor (/ linha tamanho-grupo)) tamanho-grupo))
         (c (* (floor (/ coluna tamanho-grupo)) tamanho-grupo)))
    (loop for i from 0 below tamanho-tabuleiro do
          (and (or (= numero (tabuleiro-numero tabuleiro linha i))
                   (= numero (tabuleiro-numero tabuleiro i coluna))
                   (= numero (tabuleiro-numero 
                               tabuleiro
                                (+ l (mod i tamanho-grupo))
                                (+ c (floor (/ i tamanho-grupo))))))
               (return NIL))
          finally (return T))))


;;;; Funcoes para determinar a proxima posicao a analisar
(defun posicao-vazia (tabuleiro &key (criterio #'first))
  "Funcao que recebe um 'tabuleiro' e um 'criterio' e devolve
  a posicao vazia que cumpre o 'criterio' especificado. Se
  nenhum criterio for fornecido devolve, por definicao,
  a primeira posicao encontrada que esteja vazia.
  posicao-vazia: tabuleiro x funcao -> par"
  (let ((tamanho-tabuleiro (tabuleiro-dimensao tabuleiro)))
    (if (eql criterio #'first)
      ;; No caso do criterio ser simplesmente a primeira posicao
      ;; vazia evita-se construir a lista de todas as posicoes
      ;; retornando logo que apareca a primeira.
      (loop for i from 0 below tamanho-tabuleiro do
            (let ((coluna (position 0 (nth i tabuleiro))))
              (if (not (null coluna))
                (return (cons i coluna)))))
      
      ;; No caso de ser fornecido um criterio para a seleccao da
      ;; posicao vazia, executa-se a funcao fornecida com a lista
      ;; de todas as posicoes vazias como argumento.
      (funcall criterio tabuleiro
               (loop for l from 0 below tamanho-tabuleiro append 
                     (loop for c from 0 below tamanho-tabuleiro
                           when (> (length (tabuleiro-numero tabuleiro l c)) 
                                   1)
                           collect (cons l c)))))))


;;;; Heuristica MRV (ou Minimum Remaining Values)
(defun posicao-mais-restringida (tabuleiro posicoes)
  (loop for posicao in posicoes
        for jogadas-possiveis = (length (tabuleiro-numero tabuleiro
                                                          (car posicao)
                                                          (cdr posicao)))
        with mais-restringida = NIL
        with menos-jogadas = (tabuleiro-dimensao tabuleiro)
        when (< jogadas-possiveis menos-jogadas)
        do (and (setf menos-jogadas jogadas-possiveis)
                (setf mais-restringida posicao))
        finally (return mais-restringida)))


;;;; Heuristica de Propagacao de Restricoes
(defun propaga (tabuleiro)
  (let* ((tamanho-tabuleiro (tabuleiro-dimensao tabuleiro))
         (todos-numeros (loop for i from 1 to tamanho-tabuleiro collect i))
         (novo-tabuleiro (faz-tabuleiro tamanho-tabuleiro todos-numeros)))
    (loop for i from 0 below tamanho-tabuleiro do
          (loop for j from 0 below tamanho-tabuleiro do
                (let ((numero (tabuleiro-numero tabuleiro i j)))
                  (if (and (find numero todos-numeros)
                           (not (atribui novo-tabuleiro numero i j)))
                    (return-from propaga NIL))))) 
    novo-tabuleiro))


(defun atribui (tabuleiro numero linha coluna)
  (let ((restantes-numeros 
          (remove numero (tabuleiro-numero tabuleiro linha coluna))))
    (and (loop for n in restantes-numeros
               always (elimina tabuleiro n linha coluna))
         tabuleiro)))


(defun elimina (tabuleiro numero linha coluna)
  (let ((numeros-posicao (tabuleiro-numero tabuleiro linha coluna))
        (tamanho-tabuleiro (tabuleiro-dimensao tabuleiro)))

    ;; Se o numero nao existe na posicao dada e porque ja foi eliminado.
    (if (not (find numero numeros-posicao))
      (return-from elimina tabuleiro))

    ;; Elimina o numero da linha e coluna do tabuleiro recebido como argumento.
    (setf numeros-posicao (remove numero numeros-posicao))
    (setf (nth coluna (nth linha tabuleiro)) numeros-posicao)
    
    ;; Se so ha um numero possivel para uma posicao elimina esse numero
    ;; das posicoes relacionadas. (linha, coluna, caixa)
    (cond ((= (length numeros-posicao) 0)
           (return-from elimina NIL))
          ((= (length numeros-posicao) 1)
           (or (loop for cada-posicao 
                     in (relacionadas tamanho-tabuleiro linha coluna)
                     always (elimina tabuleiro
                                     (car numeros-posicao)
                                     (car cada-posicao)
                                     (cdr cada-posicao)))
               (return-from elimina NIL))))
    
    ;; Se ha seccao onde numero aparece uma unica vez entao coloca numero
    ;; nessa posicao.
    (loop for cada-seccao in (seccoes tamanho-tabuleiro linha coluna) do
          (let ((posicoes-numero 
                  (loop for posicao in cada-seccao
                        if (find numero (tabuleiro-numero tabuleiro 
                                                          (car posicao) 
                                                          (cdr posicao)))
                        collect posicao)))
            (cond ((= (length posicoes-numero) 0)
                   (return-from elimina NIL))
                  ((= (length posicoes-numero) 1)
                   (or (atribui tabuleiro numero 
                                (caar posicoes-numero)
                                (cdr (car posicoes-numero)))
                       (return-from elimina NIL))))))

    ;; Por fim devolve o tabuleiro com as eliminacoes aplicadas.
    tabuleiro))


(defun relacionadas (tamanho-tabuleiro linha coluna)
  (let* ((tamanho-grupo (floor (log tamanho-tabuleiro 2)))
         (l (* (floor (/ linha tamanho-grupo)) tamanho-grupo))
         (c (* (floor (/ coluna tamanho-grupo)) tamanho-grupo)))
    (remove-if #'(lambda (x) (and (= (car x) linha) (= (cdr x) coluna)))
               (remove-duplicates
                 (loop for i from 0 below tamanho-tabuleiro append
                       (list (cons linha i)
                             (cons i coluna)
                             (cons (+ l (mod i tamanho-grupo))
                                   (+ c (floor (/ i tamanho-grupo))))))
                 :test #'(lambda (x y)
                           (and (= (car x) (car y)) (= (cdr x) (cdr y))))))))


(defun seccoes (tamanho-tabuleiro linha coluna)
  (let* ((tamanho-grupo (floor (log tamanho-tabuleiro 2)))
         (l (* (floor (/ linha tamanho-grupo)) tamanho-grupo))
         (c (* (floor (/ coluna tamanho-grupo)) tamanho-grupo))
         (posicoes-linha NIL)
         (posicoes-coluna NIL)
         (posicoes-caixa NIL))
    (loop for i from 0 below tamanho-tabuleiro do
          (and (setf posicoes-linha (cons (cons linha i) posicoes-linha))
               (setf posicoes-coluna (cons (cons i coluna) posicoes-coluna))
               (setf posicoes-caixa 
                     (cons (cons (+ l (mod i tamanho-grupo))
                                 (+ c (floor (/ i tamanho-grupo))))
                           posicoes-caixa))))
    (list posicoes-coluna posicoes-linha posicoes-caixa)))
