#lang scheme/base

(require (for-syntax scheme/base)
         scribble/eval
         scribble/manual)

; Sandbox evaluation for examples ----------------

; syntax (_ id requre-spec ...) -> eval
(define-syntax (define-eval stx)
  (syntax-case stx ()
    [(_ id require-stmt ...)
     #'(define id
         (let ([id (make-base-eval)])
           (interaction-eval 
            #:eval id 
            (begin (require scheme/pretty require-stmt ...)
                   (pretty-print-columns 40)))
           id))]))

; Provide statements -----------------------------

(provide (all-from-out scribble/eval)
         (all-from-out scribble/manual)
         define-eval)
