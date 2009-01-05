#lang scheme/base

(require (for-syntax scheme/base
                     (file "syntax-private.ss")))

(define-syntax (csv stx)
  (syntax-case stx ()
    [(_ arg)     #`(begin #,(expand-csv #'arg #f))]
    [(_ arg ...) #`(begin #,(expand-csv #'(!rows arg ...) #f))]))

; Provide statements -----------------------------

(provide js)