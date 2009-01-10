#lang scheme/base

(require (for-syntax scheme/base
                     (planet cce/scheme:1/syntax)
                     (planet untyped/unlib:3/syntax))
         (planet untyped/unlib:3/debug)
         "struct.ss"
         "type.ss")

; Syntax -----------------------------------------

(define-syntax (define-table define-stx)
  
  ; syntax -> syntax
  (define introduce-id
    (make-syntax-introducer))
  
  ; syntax -> syntax
  (define introduce-alias-id
    (make-syntax-introducer))
  
  ; syntax -> syntax
  (define (constraint-transformer stx)
    (syntax-case* stx (primary-key foreign-key unique check) symbolic-identifier=?
      [primary-key (lambda (table name . cols)
                     #`(add-table-primary-key! #,table #,name #,@cols))]
      [foreign-key (lambda (table name local foreign)
                     #`(add-table-foreign-key! #,table #,name #,local #,foreign))]
      [unique      (lambda (table name local . cols)
                     #`(add-table-unique-constraint! #,table #,name #,@cols))]
      [check       (lambda (table name expr)
                     #`(add-table-check-constraint! #,table #,name (sql #,expr))
                     (printf "WARNING: Check constraints disabled~n")
                     #'(void))]
      [_           (raise-syntax-error #f "unrecognised constraint type" define-stx stx)]))
  
  (syntax-case define-stx ()
    
    [(define-table table-id 
       ([column-id column-type column-arg ...] ...)
       ([constraint-id (constraint-type constraint-arg ...)] ...))
     
     (with-syntax ([hygienic-table-id                   (introduce-id #'table-id)]
                   [(hygienic-column-id ...)            (introduce-id #'(column-id ...))]
                   [(hygienic-constraint-id ...)        (introduce-id #'(constraint-id ...))]
                   [hygienic-table-alias-id             (introduce-alias-id #'table-id)]
                   [(hygienic-column-alias-id ...)      (introduce-alias-id #'(column-id ...))])
       
       #`(begin
           (define hygienic-table-id
             (make-table 'table-id))
           
           (define-values (hygienic-column-id ...)
             (let ([table-id hygienic-table-id])
               (values (add-table-column! table-id 'column-id column-type column-arg ...)
                       ...)))
           
           
           (define-values (hygienic-constraint-id ...)
             (let ([table-id  hygienic-table-id]
                   [column-id hygienic-column-id]
                   ...)
               (values #,@(map (lambda (type args)
                                 (apply (constraint-transformer type) (syntax->list args)))
                               (syntax->list #'(constraint-type ...))
                               (syntax->list #'((hygienic-table-id 'constraint-id constraint-arg ...) ...))))))
           
           (define hygienic-table-alias-id
             (make-table-alias 'hygienic-table-id hygienic-table-id))
           
           (define-values (hygienic-column-alias-id ...)
             (values (make-column-alias hygienic-table-alias-id hygienic-column-id)
                     ...))
           
           (define-syntax table-id
             (with-syntax ([local-id #'(list 'table 'the-table)])
               (make-set!-transformer
                (lambda (stx)
                  (syntax-case* stx (column-id ... constraint-id ...) symbolic-identifier=?
                    [(_)           #'hygienic-table-alias-id]
                    [(_ column-id) #'hygienic-column-alias-id]
                    ...
                    [(_ id . _)    (raise-syntax-error #f "undefined column" stx #'id)]
                    [_             #'hygienic-table-id]
                    [(set! _ val)  (raise-syntax-error #f "cannot assign to a table" stx #'table-id)])))))))]))

; Provide statements -----------------------------

(provide define-table)