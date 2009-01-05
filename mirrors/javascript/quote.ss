#lang scheme/base

(require net/url
         scheme/contract
         (file "../base.ss")
         (file "struct.ss"))

; Predicates -------------------------------------

; any -> boolean
(define (quotable? val)
  (or (boolean? val)
      (number? val)
      (string? val)
      (symbol? val)
      (bytes? val)
      (url? val)))

; any -> boolean
(define (javascript+quotable? val)
  (or (SourceElement? val)
      (Expression? val)
      (quotable? val)))

; any -> boolean
(define (statement+quotable? val)
  (or (Statement? val)
      (Expression? val)
      (Identifier? val)
      (quotable? val)))

; any -> boolean
(define (expression+quotable? val)
  (or (Expression? val)
      (Identifier? val)
      (quotable? val)))

; any -> boolean
(define (identifier+quotable? val)
  (or (Identifier? val)
      (symbol? val)))

; Quote procedures -------------------------------

; (U symbol Identifier) -> Identifier
; Turns symbols into Javascript identifiers.
(define (quote-identifier id)
  (cond [(Identifier? id) id]
        [(symbol? id) (make-Identifier #f id)]
        [else (contract-error "Expected (U symbol id), received ~a" id)]))

; (U quotable-literal expression) -> expression
(define (quote-expression val)
  (cond [(Expression? val) val]
        [(Identifier? val) (make-VarReference #f val)]
        [(boolean? val)    (make-BooleanLiteral #f val)]
        [(number? val)     (make-NumericLiteral #f val)]
        [(string? val)     (make-StringLiteral #f val)]
        [(symbol? val)     (make-StringLiteral #f (symbol->string val))]
        [(bytes? val)      (make-StringLiteral #f (bytes->string/utf-8 val))]
        [(url? val)        (make-StringLiteral #f (url->string val))]
        [else              (contract-error "Expected (U term boolean number string symbol bytes url), received ~a" val)]))

; (U quotable-literal statement expression) -> statement
(define (quote-statement val)
  (cond [(Expression? val) (make-ExpressionStatement #f val)]
        [(Statement? val)  val]
        [else              (quote-statement (quote-expression val))]))

(define (quote-javascript val)
  (cond [(Expression? val)    (make-ExpressionStatement #f val)]
        [(SourceElement? val) val]
        [else                 (quote-statement (quote-expression val))]))

; (listof term+quotable) -> statement
(define (wrap-begin items)
  (cond [(null? items)       (make-BeginStatement #f null)]
        [(null? (cdr items)) (quote-javascript (car items))]
        [else                (make-BeginStatement #f (map quote-javascript items))]))

; (listof term+quotable) -> statement
(define (wrap-block items)
  (make-BlockStatement #f (map quote-javascript items))
  (cond [(null? items)       (make-BlockStatement #f (list (make-EmptyStatement #f)))]
        [(null? (cdr items)) (quote-javascript (car items))]
        [else                (make-BlockStatement #f (map quote-javascript items))]))

; expression -> expression
(define (parenthesize-anonymous-function item)
  (if (FunctionExpression? item)
      (make-ParenExpression #f item)
      item))

; Helpers ----------------------------------------

;; string any ... -> void | exn:fail:contract
(define (contract-error format-string . args)
  (raise-exn exn:fail:contract
    (apply format format-string args)))

; Provide statements -----------------------------

(provide/contract
 [quotable?                       procedure?]
 [javascript+quotable?            procedure?]
 [statement+quotable?             procedure?]
 [expression+quotable?            procedure?]
 [identifier+quotable?            procedure?]
 [quote-identifier                (-> identifier+quotable? Identifier?)]
 [quote-expression                (-> expression+quotable? Expression?)]
 [quote-statement                 (-> statement+quotable? Statement?)]
 [quote-javascript                (-> javascript+quotable? SourceElement?)]
 [wrap-begin                      (-> (listof javascript+quotable?) SourceElement?)]
 [wrap-block                      (-> (listof javascript+quotable?) SourceElement?)]
 [parenthesize-anonymous-function (-> Expression? Expression?)])
