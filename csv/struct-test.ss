#lang mzscheme

(require "../test-base.ss"
         "struct.ss")

; Tests ----------------------------------------

; test-suite
(define/provide-test-suite struct-tests
  
  (test-equal? "single row with list arguments"
    (row (cell 1)
         (list (cell 2)
               (list (cell 3))
               (cell 4))
         (cell 5))
    (row (cell 1)
         (cell 2) 
         (cell 3)
         (cell 4)
         (cell 5))))
