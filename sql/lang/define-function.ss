#lang scheme/base

(require "../../base.ss")

(require (for-syntax scheme/base
                     scheme/list
                     scheme/pretty
                     (cce-scheme-in syntax)
                     (unlib-in symbol syntax)
                     "identifier.ss")
         (only-in scheme/list filter-map)
         "struct.ss")

; (_ id expr)
; (_ (id arg ...) expr ...)
(define-syntax (define/provide-function stx)
  (syntax-case stx ()
    [(_ header #:type type expr ...)
     (with-syntax ([id (define-header->id #'header)])
       (quasisyntax/loc stx
         (begin (define-sql header #:type 'function expr ...)
                (provide id))))]))

; symbol
(define undefined
  (gensym 'sql-undefined-arg))

; any -> boolean
(define (undefined? val)
  (eq? undefined val))

; (_ (id arg ...) [boolean-expr type] ...)
(define-syntax (define-function/types stx)
  
  (define (args->arg-formals stx)
    (filter-map (lambda (stx)
                  (syntax-case stx ()
                    [[id] (identifier? #'id)              #'[id undefined]]
                    [kw   (keyword? (syntax->datum #'kw)) #'kw]
                    [id   (identifier? #'id)              #'id]))
                (syntax->list stx)))
  
  (define (args->arg-ids stx)
    (filter-map (lambda (stx)
                  (syntax-case stx ()
                    [(id) (identifier? #'id)              #'id]
                    [kw   (keyword? (syntax->datum #'kw)) #f]
                    [id   (identifier? #'id)              #'id]))
                (syntax->list stx)))
  
  (define (args->arg-exprs stx)
    (filter-map (lambda (stx)
                  (syntax-case stx ()
                    [(id) (identifier? #'id)              #'(if (undefined? id) #f (sql-quote id))]
                    [kw   (keyword? (syntax->datum #'kw)) #f]
                    [id   (identifier? #'id)              #'(sql-quote id)]))
                (syntax->list stx)))
  
  (syntax-case stx ()
    [(_ (id arg ...) [rule type] ...)
     (identifier? #'id)
     (with-syntax ([(formal ...) (args->arg-formals #'(arg ...))]
                   [(arg    ...) (args->arg-ids     #'(arg ...))]
                   [(expr   ...) (args->arg-exprs   #'(arg ...))])
       (syntax/loc stx
         (define-sql-form (id formal ...)
           (let ([arg expr] ...)
             (make-function (type-rules [rule type] ...) 'id (list arg ...))))))]
    [(_ (id arg ... . rest) [rule type] ...)
     (identifier? #'id)
     (with-syntax ([(formal ...) (args->arg-formals #'(arg ...))]
                   [(arg    ...) (args->arg-ids     #'(arg ...))]
                   [(expr   ...) (args->arg-exprs   #'(arg ...))])
       (syntax/loc stx
         (define-function (id formal ... . rest)
           (let ([arg expr] ... [rest (map sql-quote rest)])
             (make-function (type-rules [rule type] ...) 'id (list* arg ... rest))))))]))

; (_ [bool-expr type] ...)
(define-syntax (type-rules stx)
  (syntax-case stx ()
    [(_ [rule type] ...)
     #'(cond [rule type] ...
             [else (error "function not defined for the supplied types")])]))

; (_ arg ...)
; (_ arg ... . rest)
; where arg  : id
;              [id any ...]
;       rest : id
(define-syntax (function/c stx)
  
  ; (listof syntax) -> (listof syntax)
  (define (arg-contracts stxs)
    (filter-map (lambda (stx)
                  (syntax-case stx ()
                    [(id) #f]
                    [id   #'sql-quotable?]))
                stxs))
  
  ; (listof syntax) -> (listof syntax)
  (define (opt-arg-contracts stxs)
    (filter-map (lambda (stx)
                  (syntax-case stx ()
                    [(id) #'sql-quotable?]
                    [id   #f]))
                stxs))
  
  (syntax-case stx ()
    [(_ arg ...)
     (with-syntax ([(arg-contract ...)     (arg-contracts     (syntax->list #'(arg ...)))]
                   [(opt-arg-contract ...) (opt-arg-contracts (syntax->list #'(arg ...)))])
       (syntax/loc stx
         (->* (arg-contract ...) (opt-arg-contract ...) function?)))]
    [(_ arg ... . rest)
     (with-syntax ([(arg-contract ...)     (arg-contracts     (syntax->list #'(arg ...)))]
                   [(opt-arg-contract ...) (opt-arg-contracts (syntax->list #'(arg ...)))])
       (syntax/loc stx
         (->* (arg-contract ...) (opt-arg-contract ...) #:rest (listof sql+quotable?) function?)))]))

; Provide statements -----------------------------

(provide define-function
         define-function/types
         type-rules
         function/c
         undefined
         undefined?)
