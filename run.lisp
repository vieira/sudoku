;;;; Atalho Sudoku
(load "sudoku.lisp")
(compile-file "sudoku.lisp")
(defun resolve (ficheiro)
  (let ((problema (le-tabuleiro ficheiro)))
    (escreve-tabuleiro (no-tabuleiro (procura-profundidade 
                                   (make-no :tabuleiro problema) 
                                   #'objectivo 
                                   #'sucessores)))))

