#lang scheme/base

(require (for-syntax scheme/base))

; (hasheqof symbol (syntax -> syntax)
(define expression-transformers
  (make-hasheq))

; Helpers ----------------------------------------

(define-for-syntax (make-fixed-arity-transformer stx id plain-id args)
  (with-syntax ([id       id]
                [plain-id plain-id]
                [args     args])
    (syntax/loc stx
       (lambda (stx)
         (syntax-case stx (plain-id) symbolic-identifier=?
           [(plain-id . args) #`(id #,@(map expand-expression+unquote (syntax->list #'args)))]
           [_                  (next-expression)])))))

; (_ (sql:id arg ...) type-rule ...)
; (_ (sql:id arg ...) type-rule ... #:syntax-transformer transformer)
; (_ (sql:id . args) type-rule ...)
; (_ (sql:id . args) type-rule ... #:syntax-transformer transformer)
;
; where type-rule is one of:
;   [bolean-expression type]
;   [else              type]
;
; and transformer is a procedure of type (syntax -> syntax).
;
; Creates an SQL function and registers a syntax transformer for 
; the syntax query language as a side effect.
; 
; The syntax transformer should call (next-expression) if the 
; argument does not match the keyword or arity of the function.
(define-syntax (define-function stx)
  
  (define (remove-prefix sym)
    (let ([match (regexp-match #rx"^sql:(.*)$" (symbol->string sym))])
      (if match
          (string->symbol (cadr match))
          (error "define-function: identifier must have an 'sql:' prefix: " sym))))
  
  (syntax-case stx (else)
    
    [(_ (id arg ...) ([rule type] ...))
     #`(define-function (id arg ...)
         ([rule type] ...)
         #,(make-fixed-arity-transformer #'id #'plain-id #'(arg ...)))]
    
    [(_ (id arg ...) ([rule type] ...) transformer)
     (identifier? #'id)
     (with-syntax ([plain-id           (remove-prefix (syntax->datum #'id))]
                   [(arg-contract ...) (map (lambda _ #'quotable?) (syntax->list #'(arg ...)))])
       #'(begin (define-function id
                  (lambda (arg ...)
                    (let ([arg (sql-lift arg)] ...)
                      (make-function (cond [rule type] ...
                                           [else (error "~a not defined for the types: "
                                                        'id
                                                        (map expression-type (list arg ...)))])
                                     'plain-id
                                     (list arg ...))))
                  (-> arg-contract ... function?)
                  transformer)))]
    
    [(_ (id . args) ([rule type] ...))
     #'(define-function (id . args)
         ([rule type] ...)
         (lambda (stx)
           (syntax-case stx (plain-id) symbolic-identifier=?
             [(plain-id arg (... ...)) #`(id #,@(map expand-expression+unquote (syntax->list #'(arg (... ...)))))]
             [_                        (next-expression)])))]
    
    [(_ (id . args) ([rule type] ...) transformer)
     (identifier? #'id)
     (with-syntax ([plain-id (remove-prefix (syntax->datum #'id))])
       #'(begin (define (id . args)
                  (let ([args (map sql-lift args)])
                    (make-function (cond [rule type] ...
                                         [else (error (format "~a not defined for the types: "
                                                              'id
                                                              (map expression-type args)))])
                                   'plain-id
                                   args)))
                (register-expression-transformer! 'plain-id transformer)
                (provide/contract [rename id plain-id (->* () () #:rest (listof quotable?) function?)])))]
        
    [(_ id constructor contract transformer)
     (identifier? #'id)
     (with-syntax ([plain-id           (remove-prefix (syntax->datum #'id))]
                   [(arg-contract ...) (map (lambda _ #'quotable?) (syntax->list #'(arg ...)))])
       #'(begin (define id constructor)
                (register-expression-transformer! 'plain-id transformer)
                (provide/contract [rename id plain-id contract])))]))
