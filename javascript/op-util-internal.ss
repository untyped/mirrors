#lang scheme/base

(require (only-in srfi/1/list append-map iota make-list)
         srfi/26/cut
         (planet untyped/unlib/syntax)
         (file "op.ss")
         (for-template scheme/base
                       scheme/contract
                       (only-in srfi/1/list drop-right take-right)
                       (planet untyped/unlib/syntax)
                       (file "struct.ss")
                       (file "quote.ss")))

; (syntax (listof symbol) (listof symbol) (symbol expr ... -> expr) (any -> boolean?) (U integer #f) -> syntax)
(define (make-expr-constructors stx procedure-symbols operator-symbols constructor-stx predicate-stx arity)
  (with-syntax ([constructor constructor-stx]
                [predicate   predicate-stx])
    
    ; symbol symbol -> syntax
    (define (make-unary-define-statement procedure-symbol operator-symbol)
      (with-syntax ([proc (make-id stx 'js: procedure-symbol)]) 
        #`(define (proc arg) 
            #,(if (scheme-prefix-operator? procedure-symbol)
                  #`(constructor #f '#,operator-symbol (quote-expression arg))
                  #`(constructor #f (quote-expression arg) '#,operator-symbol)))))
    
    ; symbol symbol -> syntax
    (define (make-binary-define-statement procedure-symbol operator-symbol)
      (with-syntax ([proc (make-id stx 'js: procedure-symbol)]) 
        #`(define (proc arg1 arg2)
            (constructor #f (quote-expression arg1) '#,operator-symbol (quote-expression arg2)))))
    
    ; symbol symbol -> syntax
    (define (make-nary-define-statement procedure-symbol operator-symbol)
      (with-syntax ([proc (make-id stx 'js: procedure-symbol)]) 
        #`(define (proc . args)
            (cond [(null? args)        (error (format "~a: expected at least two arguments, recevied none." 'proc))]
                  [(null? (cdr args))  (error (format "~a: expected at least two arguments, recevied one: ~a." 'proc (car args)))]
                  [(null? (cddr args)) (constructor #f (quote-expression (car args)) '#,operator-symbol (quote-expression (cadr args)))]
                  [else                (let ([most (drop-right args 1)]
                                             [last (car (take-right args 1))])
                                         (constructor #f (apply proc most) '#,operator-symbol (quote-expression last)))]))))
    
    ; symbol -> syntax
    (define (make-unary-provide-statement procedure-symbol)
      (with-syntax ([proc     (make-id #f procedure-symbol)]
                    [prefixed (make-id stx 'js: procedure-symbol)])
        #`(rename prefixed proc (-> expression+quotable? predicate))))
    
    ; symbol -> syntax
    (define (make-binary-provide-statement procedure-symbol)
      (with-syntax ([proc     (make-id #f procedure-symbol)]
                    [prefixed (make-id stx 'js: procedure-symbol)])
        #`(rename prefixed proc (-> expression+quotable? expression+quotable? predicate))))
    
    ; symbol -> syntax
    (define (make-nary-provide-statement procedure-symbol)
      (with-syntax ([proc     (make-id #f procedure-symbol)]
                    [prefixed (make-id stx 'js: procedure-symbol)])
        #`(rename prefixed proc (->* () () #:rest (listof expression+quotable?) predicate))))
    
    #`(begin (begin #,@(map (case arity
                              [(1)  make-unary-define-statement]
                              [(2)  make-binary-define-statement]
                              [(#f) make-nary-define-statement])
                            procedure-symbols
                            operator-symbols)
                    (provide/contract
                     #,@(map (case arity
                               [(1)  make-unary-provide-statement]
                               [(2)  make-binary-provide-statement]
                               [(#f) make-nary-provide-statement])
                             procedure-symbols))))))

; Provide statements -----------------------------

(provide make-expr-constructors)