#lang scheme/base

(require scheme/contract
         (only-in (planet dherman/javascript:8/ast) SourceElement? Property?)
         (planet dherman/javascript:8/private/syntax/ast-core)
         (planet dherman/javascript:8/private/syntax/token))

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

(provide (all-from-out (planet dherman/javascript:8/ast)
                       (planet dherman/javascript:8/private/syntax/ast-core)))

(provide/contract
 [struct (BeginStatement Statement) ([location (or/c region? false/c)]
                                     [statements (listof SourceElement?)])]
 [javascript? procedure?]
 [javascript-declaration? procedure?]
 [javascript-statement? procedure?]
 [javascript-expression? procedure?])
