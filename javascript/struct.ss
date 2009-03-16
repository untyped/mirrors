#lang scheme/base

(require scheme/contract
         (only-in (planet dherman/javascript:8/ast)
                  SourceElement?
                  Property?)
         (except-in (planet dherman/javascript:8/private/syntax/ast-core)
                    make-FunctionExpression
                    FunctionExpression
                    struct:FunctionExpression
                    FunctionExpression-name
                    FunctionExpression-args
                    FunctionExpression-body
                    FunctionExpression?)
         (planet dherman/javascript:8/private/syntax/token))

; Structures -------------------------------------

; (struct (U region #f) (listof Statement))
(define-struct (BeginStatement Statement) (statements) #:prefab)

; The code below is a hack to override Dave H's default indentation rule for
; FunctionExpression so it indents 4 characters from the last indentation 
; position rather than from the "f" in function.
; 
; We can't override Dave's default match rule for FunctionExpression so we
; define our own version of FunctionExpression instead.
; 
; Dave's code uses the symbolic names of structure types to check precedence,
; so we have to call our struct "FunctionExpression".
;
; Because the AST structs in JS.plt are prefabs, we have to distinguish between
; our FunctionExpression and the default FunctionExpression to bypass Dave's
; match rule. We do this by inserting an intermediate supertype, MirrorsExpression,
; between our FunctionExpression and Expression.

; (struct (U region #f))
(define-struct (MirrorsExpression Expression) () #:prefab)

; (struct (U region #f) (U Identifier #f) (listof Identifier) (listof SourceElement))
(define-struct (FunctionExpression MirrorsExpression) (name args body) #:prefab)

; Procedures -------------------------------------

; statement -> void
(define (empty-begin? stmt)
  (and (BeginStatement? stmt)
       (andmap empty-begin? (BeginStatement-statements stmt)))) 

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
 [struct (BeginStatement Statement)      ([location   (or/c region? #f)]
                                          [statements (listof SourceElement?)])]
 [struct (FunctionExpression Expression) ([location   (or/c region? #f)]
                                          [name       (or/c Identifier? #f)]
                                          [args       (listof Identifier?)]
                                          [body       (listof SourceElement?)])]
 [empty-begin?            procedure?]
 [javascript?             procedure?]
 [javascript-declaration? procedure?]
 [javascript-statement?   procedure?]
 [javascript-expression?  procedure?])
