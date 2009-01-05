#lang scheme/base

(require (for-syntax scheme/base)
         "render.ss"
         "syntax.ss")

; Procedures -------------------------------------

; (_ boolean xml ...)
(define-syntax-rule (opt-js test expr ...)
  (if test
      (js expr ...)
      (js)))

; (_ string xml ...)
(define-syntax-rule (js-debug msg expr ...)
  (let ([ans (js expr ...)])
    (printf "----------~a:~n~a~n----------~n" msg (javascript->pretty-string ans))
    ans))

; Provide statements -----------------------------

(provide opt-js
         js-debug)
