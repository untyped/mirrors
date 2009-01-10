#lang scheme/base

(require (for-syntax scheme/base
                     (file "syntax-internal.ss")))

(define-syntax (js stx)
  (syntax-case stx ()
    [(_ arg)     #`(begin #,(expand-javascript #'arg #f))]
    [(_ arg ...) #`(begin #,(expand-javascript #'(!begin arg ...) #f))]))

; Provide statements -----------------------------

(provide js)