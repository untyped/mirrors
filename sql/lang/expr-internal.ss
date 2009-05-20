#lang scheme/base

(require "../../base.ss")

(require (for-syntax scheme/base
                     scheme/list
                     (cce-scheme-in syntax)
                     (unlib-in symbol syntax))
         srfi/19
         (unlib-in list symbol)
         "struct.ss"
         "type.ss")

; Function transformers --------------------------

(define (function 

; define-function --------------------------------

(define-syntax (define-function stx)
  (syntax-case stx ()
    [(_ (id arg ...) [rule type] ...) #'(define-function (id arg ...) #:constructor make-function [rule type] ...)]
    [(_ (id . args)  [rule type] ...) #'(define-function (id . args)  #:constructor make-function [rule type] ...)]
    [(_ (id arg ...) #:constructor make-fn [rule type] ...)
     (identifier? #'id)
     (with-syntax* ([(opt-arg ...)          (filter-map (lambda (stx)
                                                          (and (not (identifier? stx))
                                                               (car (syntax->list stx))))
                                                        (syntax->list #'(arg ...)))]
                    [(arg ...)              (filter-map (lambda (stx)
                                                          (and (identifier? stx) stx))
                                                        (syntax->list #'(arg ...)))]
                    [sql-id                 (make-id #'id 'sql- #'id)]
                    [gensym-prefix          (symbol-append (syntax->datum #'sql-id) '-undefined-arg)]
                    [min-args               (length (syntax->list #'(arg ...)))]
                    [max-args               (length (syntax->list #'(arg ... opt-arg ...)))]
                    [(arg-contract ...)     (map (lambda _ #'sql-quotable?) (syntax->list #'(arg ...)))]
                    [(opt-arg-contract ...) (map (lambda _ #'sql-quotable?) (syntax->list #'(opt-arg ...)))])
       (function-transformer-set! (syntax->datum #'id) #'sql-id min-args max-args)
       #'(begin (define undefined
                  (gensym 'gensym-prefix))
                (define (sql-id arg ... [opt-arg undefined] ...)
                  (let ([arg      (sql-quote arg)] ...
                        [opt-arg  (and (not (eq? opt-arg undefined)) (sql-quote opt-arg))] ...
                        [all-args (list* arg ... (filter (lambda (x) x) (list opt-arg ...)))]
                        [ans-type (cond [rule type] ...
                                        [else (raise-type-error 'id "function not defined for the supplied types"
                                                                (map expression-type all-args))])])
                    (make-fn ans-type 'id all-args)))
                (provide/contract [sql-id (->* (arg-contract ...) (opt-arg-contract ...) function?)])))]
    [(_ (id arg ... . rest) #:constructor make-fn [rule type] ...)
     (identifier? #'id)
     (with-syntax* ([(opt-arg ...)          (filter (lambda (stx)
                                                      (and (not (identifier? stx))
                                                           (car (syntax->list stx))))
                                                    (syntax->list #'(arg ...)))]
                    [(arg ...)              (filter (lambda (stx)
                                                      (identifier? stx))
                                                    (syntax->list #'(arg ...)))]
                    [sql-id                 (make-id #'id 'sql- #'id)]
                    [gensym-prefix          (symbol-append (syntax->datum #'sql-id) '-undefined-arg)]
                    [min-args               (length (syntax->list #'(arg ...)))]
                    [max-args               (length (syntax->list #'(arg ... opt-arg ...)))]
                    [(arg-contract ...)     (map (lambda _ #'sql-quotable?) (syntax->list #'(arg ...)))]
                    [(opt-arg-contract ...) (map (lambda _ #'sql-quotable?) (syntax->list #'(opt-arg ...)))])
       (function-transformer-set! (syntax->datum #'id) #'sql-id min-args max-args)
       #'(begin (define undefined
                  (gensym 'gensym-prefix))
                (define (sql-id arg ... [opt-arg undefined] ...)
                  (let ([arg      (sql-quote arg)] ...
                        [opt-arg  (and (not (eq? opt-arg undefined)) (sql-quote opt-arg))] ...
                        [rest     (map sql-quote rest)]
                        [all-args (append (list arg ...)
                                          (filter (lambda (x) x) (list opt-arg ...))
                                          rest)]
                        [ans-type (cond [rule type] ...
                                        [else (raise-type-error 'id "function not defined for the supplied types"
                                                                (map expression-type all-args))])])
                    (make-fn ans-type 'id all-args)))
                (provide/contract [sql-id (->* (arg-contract ...) (opt-arg-contract ...) #:rest (listof sql-quotable?) function?)])))]))
