#lang scheme

(require "../base.ss")

(require (for-syntax "../base.ss"
                     "syntax-internal.ss"))

; Lowercase --------------------------------------

(define-syntax (js stx)
  (parameterize ([quote-case-restriction 'lower])
    (syntax-case stx ()
      [(_ arg)     #`(begin #,(expand-javascript #'arg #f))]
      [(_ arg ...) #`(begin #,(expand-javascript #'(!begin arg ...) #f))])))

; (_ boolean xml ...)
(define-syntax-rule (opt-js test expr ...)
  (if test
      (js expr ...)
      (js)))

; (_ string xml ...)
(define-syntax-rule (debug-js msg expr ...)
  (let ([ans (js expr ...)])
    (printf "----------~a:~n~a~n----------~n" msg (javascript->pretty-string ans))
    ans))

; Uppercase --------------------------------------

(define-syntax (JS stx)
  (parameterize ([quote-case-restriction 'upper])
    (syntax-case stx ()
      [(_ arg)     #`(begin #,(expand-javascript #'arg #f))]
      [(_ arg ...) #`(begin #,(expand-javascript #'(!begin arg ...) #f))])))

; (_ boolean xml ...)
(define-syntax-rule (OPT-JS test expr ...)
  (if test
      (JS expr ...)
      (JS)))

; (_ string xml ...)
(define-syntax-rule (DEBUG-JS msg expr ...)
  (let ([ans (JS expr ...)])
    (printf "----------~a:~n~a~n----------~n" msg (javascript->pretty-string ans))
    ans))

; Provide statements -----------------------------

(provide js opt-js debug-js
         JS OPT-JS DEBUG-JS)
