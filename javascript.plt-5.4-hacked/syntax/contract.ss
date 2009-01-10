;; This is a bogus drop-in replacement for the standard (lib "contract.ss")
;; for when compilation becomes so unwieldy that I want to ignore contracts.
(module contract mzscheme
  (require (lib "contract.ss"))
  (require-for-syntax (lib "list.ss" "srfi" "1"))

  (define-syntax (provide/contract* stx)
    (define (erase-contracts clause)
      (syntax-case clause (struct)
        [(struct (type-name parent-type-name) ())
         #'(struct type-name ())]
        [(struct (type-name parent-type-name) ([field contract] ...))
         (let* ([type (syntax-local-value #'type-name)]
                [supertype (and (list-ref type 5) (syntax-local-value (list-ref type 5)))]
                [accessors (list-ref type 3)]
                [supertype-accessors (and supertype (list-ref supertype 3))])
           (if (and supertype (last accessors))
               (with-syntax ([(good-field ...) (take-right (syntax->list #'(field ...))
                                                           (- (length accessors) (length supertype-accessors)))])
                 #'(struct type-name (good-field ...)))
               (raise-syntax-error 'provide/contract "not enough static information about struct type" stx clause)))]
        [(struct type-name ([field contract] ...))
         #'(struct type-name (field ...))]
        [[id contract]
         #'id]))
    (syntax-case stx ()
      [(_ clause ...)
       (with-syntax ([(provide-clause ...)
                      (map erase-contracts (syntax->list #'(clause ...)))])
         #'(provide provide-clause ...))]))

  (provide (all-from-except (lib "contract.ss") provide/contract)
           (rename provide/contract* provide/contract)))
