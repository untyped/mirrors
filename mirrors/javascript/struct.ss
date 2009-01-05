#lang scheme/base

(require scheme/contract
         (planet cobbe/contract-utils/contract-utils)
         (file "../javascript.plt-5.4-hacked/syntax/ast.ss")
         (file "../javascript.plt-5.4-hacked/syntax/token.ss"))

; Structures -------------------------------------

(define-struct (BeginStatement Statement) (statements) #:transparent)

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

(provide (all-from-out (file "../javascript.plt-5.4-hacked/syntax/ast.ss")))

(provide/contract
 [struct (BeginStatement Statement) ([location (optional/c region?)]
                                     [statements (listof SourceElement?)])]
 [javascript? procedure?]
 [javascript-declaration? procedure?]
 [javascript-statement? procedure?]
 [javascript-expression? procedure?])
