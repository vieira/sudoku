;;;; Atalho Sudoku
(compile-file "sudoku.lisp" :output-file "sudoku.fas")
(load "sudoku.fas")
(defun resolve (ficheiro estrategia)
  (escreve-tabuleiro (procura ficheiro estrategia)))
