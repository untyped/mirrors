#lang scheme/base

(require (for-syntax scheme/base)
         "../javascript.ss"
         "../struct.ss")

;(define (extract-requires stx accum)
;  (let loop ([stx stx] [req-accum null] [stmt-accum null])
;    (syntax-case* stx (require) symbolic-identifer=?
;      [((require arg ...) req+stmt ...)
;       (loop #'(req+stmt ...)
;             (cons #'(require arg ...) req-accum)
;             stmt-accum)]
;      [(stmt req+stmt ...)
;       (loop #'(req+stmt ...)
;             req-accum
;             (cons #'stmt stmt-accum))]
;      [() #`(#,@(reverse req-accum)
;             #,@(reverse stmt-accum))])))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(module-begin) #'(#%plain-module-begin (begin #f))]
    [(module-begin stmt ...)
     #'(#%plain-module-begin
        (define ans (make-BeginStatement #f (list stmt ...)))
        (display (javascript->pretty-string ans))
        (provide ans))]))

(provide (rename-out [module-begin #%module-begin])
         (except-out (all-from-out scheme/base) #%module-begin))