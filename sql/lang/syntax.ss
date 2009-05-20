#lang scheme/base

(require (for-syntax scheme/base
                     "syntax-internal.ss")
         "identifier.ss"
         "expression.ss")

; Syntax -----------------------------------------

(define-syntax (sql stx)
  (syntax-case stx ()
    [(_ expr) (expand-top-level (syntax expr))]))

; Provide statements -----------------------------

(provide sql)

(sql (+ 1 2 3))
