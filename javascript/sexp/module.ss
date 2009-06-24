#lang scheme/base

(require (for-syntax scheme/base
                     (planet untyped/unlib:3/debug)
                     (planet untyped/unlib:3/syntax))
         "../javascript.ss"
         "../javascript-registry.ss")

(define-for-syntax (extract-requires stx)
  (let loop ([stx stx] [req-accum null] [stmt-accum null])
    (syntax-case* stx (require) symbolic-identifier=?
      [((require arg ...) req+stmt ...)
       (loop #'(req+stmt ...)
             (cons #'(require arg ...) req-accum)
             stmt-accum)]
      [(stmt req+stmt ...)
       (loop #'(req+stmt ...)
             req-accum
             (cons #'stmt stmt-accum))]
      [() #`(#,(reverse req-accum)
             #,(reverse stmt-accum))])))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(module-begin) #'(#%plain-module-begin (begin #f))]
    [(module-begin req+stmt ...)
     (with-syntax ([script (datum->syntax stx 'script)]
                   [((require ...) (stmt ...))
                    (extract-requires #'(req+stmt ...))])
     #'(#%plain-module-begin
        require ...
        (define script (js stmt ...))
        (display (javascript->pretty-string script))
        (registry-add! script)
        (provide script)))]))

(provide (rename-out [module-begin #%module-begin])
         (except-out (all-from-out scheme/base) #%module-begin))
