#lang scheme/base

(require "../../base.ss")

(require scheme/list
         (for-template scheme/base))

; symbol
(define sql-identifier-key 'sql-id) ; must be interned

; syntax symbol -> (U symbol #f)
(define (sql-identifier-type stx)
  (and (identifier? stx)
       (let ([proc (with-handlers ([exn? (lambda _ #f)])
                     (syntax-local-value stx #f))])
         (and (procedure? proc)
              (let ([val (proc)])
                (and (member sql-identifier-key (syntax-property-symbol-keys val))
                     (syntax-property val sql-identifier-key)))))))

; syntax symbol -> syntax
(define (make-sql-identifier stx type)
  (syntax-property stx sql-identifier-key type))

; syntax syntax -> ([syntax] -> syntax)
(define (make-sql-transformer from-stx to-stx)
  
  ; syntax -> natural
  (define (args->min-arity stx)
    (length (filter-map (lambda (stx)
                          (and (identifier? stx)
                               (not (keyword? stx))))
                        (syntax->list stx))))
  
  ; syntax -> natural
  (define (args->max-arity stx)
    (length (filter-map (lambda (stx)
                          (not (keyword? stx)))
                        (syntax->list stx))))
  
  ; (U natural #f)
  ; (U natural #f)
  (define-values (min-arity max-arity)
    (syntax-case from-stx ()
      [(_ arg ...)        (values (args->min-arity #'(arg ...))
                                  (args->max-arity #'(arg ...)))]
      [(_ arg ... . rest) (values (args->min-arity #'(arg ...))
                                  #f)]
      [_                  (values #f #f)]))
  
  (with-syntax ([to        to-stx]
                [min-arity min-arity]
                [max-arity max-arity])
    #'(case-lambda
        [(stx)
         (syntax-case stx ()
           [id (identifier? #'id) #'to]
           [(id arg (... ...))
            (identifier? #'id)
            (let ([arity (length (syntax->list #'(arg (... ...))))])
              (cond [(and (not min-arity) (not max-arity))
                     (raise-syntax-error #f "not a function" stx)]
                    [(and (not max-arity) (< arity min-arity))
                     (raise-syntax-error
                      #f
                      (format "incorrect arity: expected ~a or more arguments"
                              min-arity)
                      stx)]
                    [(and max-arity (or (< arity min-arity) (> arity max-arity)))
                     (raise-syntax-error
                      #f
                      (format "incorrect arity: expected ~a to ~a arguments"
                              min-arity
                              max-arity)
                      stx)]
                    [else #'(to arg (... ...))]))])]
        [()    #'to])))

; syntax -> syntax
;
; Extracts the ID from a define-style header:
;
;   (define id expr)
;   (define (id arg ...) expr ...)
;   etc...
;
; The header is the first part after the define keyword.
(define (define-header->id stx)
  (syntax-case stx ()
    [(id arg ...)        (identifier? #'id) #'id]
    [(id arg ... . rest) (identifier? #'id) #'id]
    [id                  (identifier? #'id) #'id]))

; syntax -> syntax
;
; Injects a new ID into a define-style header:
;
;   (define id expr)
;   (define (id arg ...) expr ...)
;   etc...
;
; The header is the first part after the define keyword.
(define (id->define-header id-stx header-stx)
  (with-syntax ([id id-stx])
    (syntax-case header-stx ()
      [(id0 arg ...)        (identifier? #'id0) (syntax/loc #'id0 (id arg ...))]
      [(id0 arg ... . rest) (identifier? #'id0) (syntax/loc #'id0 (id arg ... . rest))]
      [id0                  (identifier? #'id0) (syntax/loc #'id0 id)])))

; Provide statements -----------------------------

(provide/contract
 [sql-identifier-type  (-> syntax? (or/c symbol? #f))]
 [make-sql-identifier  (-> syntax? symbol? syntax?)]
 [make-sql-transformer (-> syntax? syntax? syntax?)]
 [define-header->id    (-> syntax? syntax?)]
 [id->define-header    (-> syntax? syntax? syntax?)])