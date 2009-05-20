#lang scheme/base

(require "../../base.ss")

(require "struct.ss")

; (struct symbol syntax (syntax -> syntax))
(define-struct syntax-info
  (id constructor transformer)
  #:transparent)

; -> hasheq
(define make-syntax-hash
  make-hasheq)

; any -> boolean
(define (syntax-hash? val)
  (and (hash? val)
       (hash-eq? val)))

; hasheq identifier -> syntax-info
(define (syntax-hash-ref hash id)
  (let ([id (syntax->datum id)])
    (hash-ref hash id (cut error "sql ~a: form not found" id))))

; hasheq identifier identifier syntax natural (U natural #f) -> syntax-info
(define (syntax-hash-add! hash id constructor min-arity max-arity)
  (let ([id                (syntax->datum id)]
        [raise-arity-error (lambda (stx)
                             (raise-syntax-error
                              #f
                              (cond [(equal? min-arity max-arity)
                                     (format "sql ~a expects ~a arguments" id min-arity)]
                                    [(not max-arity)
                                     (format "sql ~a expects ~a or more arguments" id min-arity)]
                                    [else
                                     (format "sql ~a expects ~a to ~a arguments" id min-arity max-arity)])
                              stx))]
        [transformer       (lambda (stx expand-arg)
                             (syntax-case stx ()
                               [(_ arg ...)
                                (let* ([args  (syntax->list #'(arg ...))]
                                       [arity (length args)])
                                  (cond [(< arity min-arity)                 (raise-arity-error stx)]
                                        [(and max-arity (> arity max-arity)) (raise-arity-error stx)]
                                        [else (with-syntax ([make constructor])
                                                #`(make #,@(map expand-arg args)))]))]))])
    (hash-set! hash id (make-syntax-info id constructor transformer))))

; identifier (syntax -> syntax) -> syntax
(define (syntax-hash-expand hash stx expand-arg)
  (syntax-case stx ()
    [(_ id arg ...)
     ((syntax-info-transformer (syntax-hash-ref hash #'id)) stx expand-arg)]))

; Provide statements -----------------------------

(provide/contract
 [struct syntax-info ([id          symbol?]
                      [constructor syntax?]
                      [transformer (-> syntax? procedure? syntax?)])]
 [make-syntax-hash   (-> syntax-hash?)]
 [syntax-hash?       (-> any/c boolean?)]
 [syntax-hash-ref    (-> syntax-hash? identifier? syntax-info?)]
 [syntax-hash-add!   (-> syntax-hash?
                         identifier?
                         identifier?
                         natural-number/c
                         (or/c natural-number/c #f)
                         void?)]
 [syntax-hash-expand (-> syntax-hash?
                         identifier?
                         (-> syntax? syntax?)
                         syntax?)])
