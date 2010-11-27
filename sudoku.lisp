;faz-tabuleiro: inteiro × inteiro ? tabuleiro
(defun faz-tabuleiro(n val)
	(faz-tabuleiro-aux n n val))
	
(defun faz-tabuleiro-aux(n size val)
	(if(> size 0)
		(cons (make-list n :initial-element val)
			  (faz-tabuleiro-aux n (- size 1) val))))

;construtor tabuleiro-poe-numero:  tabuleiro × inteiro × inteiro × inteiro ? tabuleiro
(defun tabuleiro-poe-numero(tab n l c)
	(setf novotab (copia-tabuleiro tab))
	(setf (nth c (nth l novotab)) n)
	novotab)

(defun tabuleiro-poe-numero-le(tab1 n l c)
	(let ((tab-n (copy-list tab1)))
		(setf (nth c (nth l tab-n)) n)
		tab-n))
		
	
(defun copia-tabuleiro (tab)
	(let ((tam (tabuleiro-dimensao tab))) 
	(setf novotab (faz-tabuleiro tam 0))
	(dotimes(linha tam novotab)
		(dotimes(coluna tam)
			(setf (nth coluna (nth linha novotab)) (tabuleiro-numero tab linha coluna))))))

;selector tabuleiro-numero: tabuleiro × inteiro × inteiro ? inteiro
(defun tabuleiro-numero(tab l c)
	(nth c (nth l tab)))
	
	
	
;selector tabuleiro-dimensao: tabuleiro ? inteiro
(defun tabuleiro-dimensao(tab)
	(length tab))
	
;reconhecedor tabuleiro-p: universal ? booleano
(defun tabuleiro-p (tab)
	(if(not(integerp (sqrt(tabuleiro-dimensao tab))))
		nil
	 (let ((bla) (resultado) (tam (tabuleiro-dimensao tab)))
		(dolist (bla tab resultado) 
			(if (listp bla)
				(if (= tam (length bla))
					(setf resultado T)
					(return))
			(return)
		)))))
		
;transformador de entrada le-tabuleiro: stream ? tabuleiro
(defun le-tabuleiro(stream)

; passar o with-open-file para a função procura
	(with-open-file (data stream)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(setf tam (parse-integer (read-line data)))
		(setf tab (faz-tabuleiro tam 0))
		(le-tabuleiro-aux data tab tam 0))
		tab)
		
(defun le-tabuleiro-aux(data tab tam linha)		
		(if (< linha tam)
			(let ((st 0) (linhalida (read-line data)) (col 0))
				(dotimes (col tam)
					(multiple-value-bind (int fim) (parse-integer linhalida :start st :junk-allowed t)
					(setf tab (tabuleiro-poe-numero-le tab int linha col))
					(setf st fim)))
				(le-tabuleiro-aux data tab tam (+ 1 linha)))))
						
;transformador de sa´ida escreve-tabuleiro: tabuleiro ? NIL
(defun escreve-tabuleiro(tab)
	(let ((tam (tabuleiro-dimensao tab)) (st 0))
	(format t "~S" tam)
	(dotimes (st tam)
	(format t "~%")
		(escreve-linha(nth st tab) tam)))) 

(defun escreve-linha(lst tam)1
	(if(> tam 0)
		(progn
			(format t "~S " (car lst))
			(escreve-linha (cdr lst) (- tam 1)))))	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Funções Auxiliares


(defun get-linha (tab numero-linha)
	(nth numero-linha tab))
	
(defun get-coluna (tab numero-coluna)
	(let ((tam (tabuleiro-dimensao tab)) (v 0) (lista NIL))
		(dotimes (v tam lista)
			(setf lista (append lista (list (tabuleiro-numero tab v numero-coluna)))))))
	
(defun get-grupo (tab numero-linha numero-coluna)
	(let ((tam (sqrt (tabuleiro-dimensao tab))) (l 0) (c 0) (lista NIL))
		(dotimes (l tam lista)
			(dotimes (c tam)
				(setf lista (append lista (list (tabuleiro-numero tab (+ (* (truncate numero-linha tam) tam) l) (+ (* (truncate numero-coluna tam) tam) c)))))))))
	
	

	
(defun elemento-lista-p (lista val)
		(dolist (x lista)
			(if (= x val)
				(return T))))
	
(defun is-number-valid-p (tab linha coluna val)
	(let ((lista-linha (get-linha tab linha)) (lista-col (get-coluna tab coluna)) (lista-grupo (get-grupo tab linha coluna)))
		(and (not (elemento-lista-p lista-linha val)) (not (elemento-lista-p lista-col val)) (not (elemento-lista-p lista-grupo val)))))
		
		
	
(defun primeira-posicao-vazia (no)
	(let* ((tab (no-tabuleiro no)) (tam (tabuleiro-dimensao tab)) (l 0) (c 0))
		(dotimes (l tam)
				(dotimes (c tam)
					(if (= 0 (tabuleiro-numero tab l c))
							(return-from primeira-posicao-vazia (cons l c)))))
			))
	
(defvar *lista-global* ())
			
(defun procura-profundidade2 (tab)
	(let ((posicao-vazia (tabuleiro-completo-p tab)) (n 0) (l 0) (c 0) (tam (tabuleiro-dimensao tab)) (contador 0))
	(if (eq T posicao-vazia)
	(progn
	    (print "OLEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE")
		(setf *lista-global* ())
		(escreve-tabuleiro tab)
		(return-from procura-profundidade tab))
		(progn
			(setf l (car posicao-vazia))
			(setf c (cdr posicao-vazia))
			(dotimes (n tam)
				(if (eq T (is-number-valid-p tab l c (+ 1 n)))
					(progn
					 (incf contador)
					 (setf tabnovo (tabuleiro-poe-numero tab (+ 1 n) l c))
				     ;(setf *lista-global* (cons (tabuleiro-poe-numero tab (+ 1 n) l c) *lista-global*))
					 (setf *lista-global* (cons tabnovo *lista-global*)))))
			(dotimes (x contador)
			(if (> (length *lista-global*) 0)
				(progn
					(setf tabaux (car *lista-global*))
					(setf *lista-global* (cdr *lista-global*))
					(setf res (procura-profundidade tabaux))
					(if (not *lista-global*)
						(return-from procura-profundidade res))  				
			)))))))
	

(defvar *VAR_DE_DEBUG* 0)
	
(defun procura-largura2 (tab)
	(let ((posicao-vazia (tabuleiro-completo-p tab)) (n 0) (l 0) (c 0) (tam (tabuleiro-dimensao tab))
	(lista_aux NIL))
	(incf *VAR_DE_DEBUG*)
	(print *VAR_DE_DEBUG*)
	(if (eq T posicao-vazia)
	(progn
	    (print "OLEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE")
		(setf *lista-global* ())
		(escreve-tabuleiro tab)
		(return-from procura-largura tab))
	(progn
		(setf l (car posicao-vazia))
		(setf c (cdr posicao-vazia))
		(dotimes (n tam)
			(if (eq T (is-number-valid-p tab l c (+ 1 n)))
				(progn
					(setf tabnovo (tabuleiro-poe-numero tab (+ 1 n) l c))
					(setf lista_aux (cons tabnovo lista_aux)))))
		(setf *lista-global* (append *lista-global* lista_aux))
		
		(if (> (length *lista-global*) 0)
			(progn
			(setf tabaux (car *lista-global*))
			(setf *lista-global* (cdr *lista-global*))
			(setf res (procura-largura tabaux))
			(if (not *lista-global*)
				(return-from procura-largura res))  				
		))))))		
			
			
(DEFUN FULL (MATRIX)
               (LET ((RET T))
                 (DOLIST (ROW MATRIX RET)
                   (DOLIST (COL ROW RET) (IF (= COL 0) (SETQ RET NIL))))))

(DEFUN VALIDGROUP (GROUP) (LET ((RET T)) (DOLIST (ITEM GROUP RET) (IF (AND (> (COUNT ITEM GROUP) 1) (NOT (= ITEM 0))) (SETQ RET NIL)))))

(DEFUN INT-SQRT (NUM) (LET ((VAL 0)) (SETQ VAL (FLOOR (SQRT NUM)))))

(DEFUN CHECKROWS (SUDOKU)
               (LET ((RET T))
                 (DOLIST (ROW SUDOKU RET) (IF (NOT (VALIDGROUP ROW)) (SETQ RET NIL)))))

(DEFUN CHECKCOLS (SUDOKU)
               (LET ((RET T))
                 (DOTIMES (COL (LENGTH SUDOKU) RET)
                   (LET ((TEMP '(0 0)))
                     (DOLIST (ROW SUDOKU RET)
                       (SETQ TEMP (APPEND TEMP (LIST (NTH COL ROW)))))
                     (IF (NOT (VALIDGROUP (REST (REST TEMP)))) (SETQ RET NIL))
                     (SETQ TEMP '(0 0))))))

(DEFUN CHECKBOXES (SUDOKU)
                (LET ((RET T) (BASELEN (INT-SQRT (LENGTH SUDOKU))))
                  (DOTIMES (ROWBASE BASELEN RET)
                    (DOTIMES (COLBASE BASELEN RET)
                      (LET ((TEMP '(-1 -1)))
                        (DOTIMES (I BASELEN RET)
                          (SETQ TEMP
                                (APPEND TEMP
                                        (SUBSEQ (NTH (+ I (* BASELEN ROWBASE)) SUDOKU)
                                                (* BASELEN COLBASE)
                                                (+ (* BASELEN COLBASE) BASELEN)))))
                        (IF (NOT (VALIDGROUP (REST (REST TEMP)))) (SETQ RET NIL)))))))

(DEFUN VALIDSUDOKU (SUDOKU)
                (AND (AND (FULL SUDOKU) (CHECKROWS SUDOKU))
                     (AND (CHECKCOLS SUDOKU) (CHECKROWS SUDOKU))))

(DEFUN SETELEM (LIST INDEX VAL)
                (IF (= INDEX 0)
                    (CONS VAL (REST LIST))
                  (CONS (FIRST LIST) (SETELEM (REST LIST) (- INDEX 1) VAL))))

(DEFUN SETMATRIXELEM (MATRIX ROW COL VAL)
                (IF (= ROW 0)
                    (APPEND (LIST (SETELEM (FIRST MATRIX) COL VAL))
                            (REST MATRIX))
                  (APPEND (LIST (FIRST MATRIX))
                          (SETMATRIXELEM (REST MATRIX) (- ROW 1) COL VAL))))

(DEFUN EXPAND (SUDOKU ROW COL)
               (LET ((STATES 'NIL))
                 (DOTIMES (I (LENGTH SUDOKU) STATES)
                   (SETQ STATES (APPEND STATES (LIST (SETMATRIXELEM SUDOKU ROW COL (+ 1 I))))))))

(DEFUN EXPAND-FIRST (SUDOKU)
               (LET ((LIST NIL))
                 (DOTIMES (ROW (LENGTH SUDOKU) LIST)
                   (DOTIMES (COL (LENGTH SUDOKU) LIST)
                     (IF (= 0 (NTH COL (NTH ROW SUDOKU))) (PROGN (SETQ LIST (EXPAND SUDOKU ROW COL)) (RETURN)))))))

(DEFUN BFSOLVER (PROBLEM &OPTIONAL (FRINGE '()))
               (SETQ FRINGE (APPEND FRINGE (LIST PROBLEM)))
               (DO ((J 0 (+ J 1)))
                   (NIL)
                 (IF (NULL FRINGE) (RETURN NIL))
                 (LET ((CUR (FIRST FRINGE)))
                   (IF (VALIDSUDOKU CUR) (RETURN CUR))
                   (SETQ FRINGE (APPEND FRINGE (EXPAND-FIRST CUR)))
                   (SETQ FRINGE (REST FRINGE)))))

(DEFUN DFSOLVER (PROBLEM &OPTIONAL (FRINGE '()))
                (SETQ FRINGE (APPEND FRINGE (LIST PROBLEM)))
                (DO ((J 0 (+ J 1)))
                    (NIL)
                  (IF (NULL FRINGE) (RETURN NIL))
                  (LET ((CUR (FIRST FRINGE)))
                    (IF (VALIDSUDOKU CUR) (RETURN CUR))
                    (SETQ FRINGE (REST FRINGE))
                    (SETQ FRINGE (APPEND (EXPAND-FIRST CUR) FRINGE)))))

(DEFUN CHECKCOL (SUDOKU COL)
                (LET ((RET T) (TEMP '(0 0)))
                  (DOLIST (ROW SUDOKU RET) (SETQ TEMP (APPEND TEMP (LIST (NTH COL ROW)))))
                  (IF (NOT (VALIDGROUP (REST (REST TEMP)))) (SETQ RET NIL))
                  RET))

(DEFUN CHECKBOX (SUDOKU ROW COL)
                (LET ((BASELEN (INT-SQRT (LENGTH SUDOKU))) (RET T) (TEMP '(0 0)))
                  (LET ((ROWBASE (FLOOR (/ ROW BASELEN))) (COLBASE (FLOOR (/ COL BASELEN))))
                    (DOTIMES (I BASELEN RET)
                      (SETQ TEMP (APPEND TEMP (SUBSEQ (NTH (+ I (* BASELEN ROWBASE)) SUDOKU) (* BASELEN COLBASE) (+ (* BASELEN COLBASE) BASELEN)))))
                    (IF (NOT (VALIDGROUP (REST (REST TEMP)))) (SETQ RET NIL))
                    RET)))

(DEFUN GETALLOWEDVALUES (SUDOKU ROW COL)
                (LET ((VALS '(0 0)))
                  (DOTIMES (I (LENGTH SUDOKU) (REST (REST VALS)))
                    (LET ((TEMP (SETMATRIXELEM SUDOKU ROW COL (+ 1 I))))
                      (IF (AND (AND (VALIDGROUP (NTH ROW TEMP)) (CHECKCOL TEMP COL)) (CHECKBOX TEMP ROW COL))
                          (SETQ VALS (APPEND VALS (LIST (+ 1 I)))))))))

(DEFUN EXPAND-ALLOWED (SUDOKU ROW COL)
                (LET ((STATES 'NIL))
                  (DOLIST (I (GETALLOWEDVALUES SUDOKU ROW COL) STATES) (SETQ STATES (APPEND STATES (LIST (SETMATRIXELEM SUDOKU ROW COL I)))))))

(DEFUN EXPAND-BEST (SUDOKU)
                (LET ((LIST 'NIL) (BESTROW -1) (BESTCOL -1) (BESTVALS 'NIL))
                  (DOTIMES (I (+ 1 (LENGTH SUDOKU))) (SETQ BESTVALS (APPEND BESTVALS (LIST I))))
                  (DOTIMES (ROW (LENGTH SUDOKU) LIST)
                    (DOTIMES (COL (LENGTH SUDOKU) LIST)
                      (IF (= 0 (NTH COL (NTH ROW SUDOKU)))
                          (LET ((TEMPVALS (GETALLOWEDVALUES SUDOKU ROW COL)))
                            (IF (< (LENGTH TEMPVALS) (LENGTH BESTVALS))
                                (PROGN (PROGN (SETQ BESTROW ROW) (SETQ BESTCOL COL)) (SETQ BESTVALS TEMPVALS)))))))
                  (SETQ LIST (EXPAND-ALLOWED SUDOKU BESTROW BESTCOL))
                  LIST))

(DEFUN SMARTBFSOLVER (PROBLEM &OPTIONAL (FRINGE '()))
               (SETQ FRINGE (APPEND FRINGE (LIST PROBLEM)))
               (DO ((J 0 (+ J 1)))
                   (NIL)
                 (IF (NULL FRINGE) (RETURN NIL))
                 (LET ((CUR (FIRST FRINGE)))
                   (IF (VALIDSUDOKU CUR) (RETURN CUR))
                   (SETQ FRINGE (APPEND FRINGE (EXPAND-BEST CUR)))
                   (SETQ FRINGE (REST FRINGE)))))

(DEFUN SMARTDFSOLVER (PROBLEM &OPTIONAL (FRINGE '()))
                (SETQ FRINGE (APPEND FRINGE (LIST PROBLEM)))
                (DO ((J 0 (+ J 1)))
                    (NIL)
                  (IF (NULL FRINGE) (RETURN NIL))
                  (LET ((CUR (FIRST FRINGE)))
                    (IF (VALIDSUDOKU CUR) (RETURN CUR))
                    (SETQ FRINGE (REST FRINGE))
                    (SETQ FRINGE (APPEND (EXPAND-BEST CUR) FRINGE)))))
		

;;;; Definição da estrutura nó
(defstruct no tabuleiro posicao jogadas)
		
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
  
(defun prepend (a b) "Coloca b no inicio a" (append b a))

;;;; Funções de suporte à procura em árvore específicas do problema
(defun objectivo (estado)
  "Verifica se estado é o estado objectivo do jogo."
  (let ((jogo-lista (loop for linha in (no-tabuleiro estado) append linha)))
      (loop for valor in jogo-lista 
            never (zerop valor))))

;		(progn
;			(setf l (car posicao-vazia))
;			(setf c (cdr posicao-vazia))
;			(dotimes (n tam)
;				(if (eq T (is-number-valid-p tab l c (+ 1 n)))
;					(progn
;					 (incf contador)
;					 (setf tabnovo (tabuleiro-poe-numero tab (+ 1 n) l c))
;					 (setf *lista-global* (cons tabnovo *lista-global*))))
			
(defun sucessores (actual)
  "Gera uma lista de nós sucessores do no nó actual dado, tendo em conta
  as regras do jogo e as possíveis próximas jogadas."
  (let ((posicao-vazia (primeira-posicao-vazia actual)))
          (loop for i from 1 to (tabuleiro-dimensao (no-tabuleiro actual))
                   if (is-number-valid-p (no-tabuleiro actual) (car posicao-vazia) (cdr posicao-vazia) i)
                   collect (make-no 
                             :tabuleiro (tabuleiro-poe-numero (no-tabuleiro actual) i (car posicao-vazia) (cdr posicao-vazia))
                             :posicao NIL
                             :jogadas NIL))))

							 
							 
(defun raiz (jogo)
  "Recebe a matriz relativa ao estado actual do jogo e devolve um nó
  a ser expandido, quando um dos extremos foi alcançado."
  (make-no :tabuleiro jogo
           :posicao NIL
           :jogadas NIL))
