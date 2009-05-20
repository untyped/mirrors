#lang scheme/base

(require "../../base.ss")

(require (for-syntax scheme/base
                     (cce-scheme-in syntax)
                     (unlib-in syntax)
                     "identifier-internal.ss")
         "identifier-internal.ss")

; Binds an identifier with the corresponding SQL #:type:
;
;   - 'expression - can be used in expressions;
;   - 'source     - can be used in source expressions;
;   - etc...
(define-syntax (define/sql stx)
  (syntax-case stx ()
    [(_ header0 #:type type expr ...)
     (with-syntax* ([id0     (define-header->id #'header0)]
                    [private (make-sql-identifier (make-id #f #'id0 '-private) (syntax->datum #'type))]
                    [header  (id->define-header #'private #'header0)])
       (quasisyntax/loc stx
         (begin (define header expr ...)
                (define-syntax id0 #,(make-sql-transformer #'header #'private)))))]))

; (_ id expr)
; (_ (id arg ...) expr ...)
(define-syntax (define-syntax/sql stx)
  (syntax-case stx ()
    [(_ header #:type type expr ...)
     (with-syntax* ([id      (define-header->id #'header)]
                    [private (make-sql-identifier (make-id #f #'id '-private) (syntax->datum #'type))]
                    [header  (id->define-header #'private #'header)])
       (quasisyntax/loc stx
         (begin (define-syntax header expr ...)
                (define-syntax id #,(make-sql-transformer #'header #'private)))))]))

(define-syntax (debug-id stx)
  (syntax-case stx ()
    [(_ id) (begin (display (sql-identifier-type #'id))
                   (newline)
                   #'(void))]))

; Provide statements -----------------------------

(provide define/sql
         define-syntax/sql
         make-sql-identifier
         make-sql-transformer
         sql-identifier-type
         define-header->id
         id->define-header)
