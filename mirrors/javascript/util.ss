#lang scheme/base

(require (for-syntax scheme/base)
         (file "syntax.ss"))

; Procedures -------------------------------------

; (_ boolean xml)
(define-syntax (opt-js stx)
  (syntax-case stx ()
    [(opt-js test expr ...)
     #'(if test
           (js expr ...)
           (js))]))

; Provide statements -----------------------------

(provide opt-js)
