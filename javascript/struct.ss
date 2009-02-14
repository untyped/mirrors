#lang scheme/base

(require "../base.ss")

(require (only-in (javascript-in ast)
                  SourceElement?
                  Property?)
         (javascript-in [private/syntax/ast-core
                         private/syntax/token]))

; Structures -------------------------------------

(define-struct (BeginStatement Statement) (statements) #:prefab)

; Procedures -------------------------------------

; any -> boolean
(define (javascript? item)
  (or (SourceElement? item)
      (Expression? item)))

; any -> boolean
(define javascript-declaration?
  Declaration?)

; any -> boolean
(define javascript-statement?
  Statement?)

; any -> boolean
(define javascript-expression?
  Expression?)

; Provide statements -----------------------------

(provide (javascript-out [ast private/syntax/ast-core]))

(provide/contract
 [struct (BeginStatement Statement) ([location (or/c region? false/c)]
                                     [statements (listof SourceElement?)])]
 [javascript? procedure?]
 [javascript-declaration? procedure?]
 [javascript-statement? procedure?]
 [javascript-expression? procedure?])
