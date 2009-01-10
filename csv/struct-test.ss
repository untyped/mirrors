#lang mzscheme

(require (file "../test-base.ss")
         (file "struct.ss"))

; Tests ----------------------------------------

; test-suite
(define struct-tests
  (test-suite "struct.ss"
    
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
           (cell 5)))))

; Provide statements ---------------------------

(provide struct-tests)
