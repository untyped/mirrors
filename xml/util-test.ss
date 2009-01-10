#lang scheme/base

(require (file "../javascript/javascript.ss")
         (file "../test-base.ss")
         (file "struct.ss")
         (file "util.ss"))

; Tests ------------------------------------------

(define util-tests
  (test-suite "util.ss"
    
    (test-equal? "alist->attributes"
      (alist->attributes `((title . "title")
                           (rowspan . 2)
                           (onclick . ,(js (alert "clicked")))))
      (list (make-attribute 'title "title")
            (make-attribute 'rowspan 2)
            (make-attribute 'onclick (js (alert "clicked")))))
    
    ))

; Provide statements -----------------------------

(provide util-tests)