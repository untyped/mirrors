#lang scheme/base

(require "../../test-base.ss"
         "define-table.ss"
         "struct.ss"
         "type.ss")

(define-table employer
  ([id              (make-integer-type)]
   [name            (make-string-type 32)])
  ([primary-key     (primary-key id)]))

(define-table person
  ([id              (make-integer-type)]
   [name            (make-string-type 32)]
   [age             (make-integer-type)]
   [employer-id     (make-integer-type)])
  ([primary-key     (primary-key id)]
   [name-age-unique (unique name age)]
   [employer-ref    (foreign-key employer-id (employer id))]
   [age-check       (check (> age 0))]))

(define define-table-tests
  (test-suite "define-table.ss"
    
    (test-case "resolve table"
      (check-pred table? employer)
      (check-pred table? person)
      (check-not-eq? employer person))
    
    (test-case "resolve table alias"
      (let ([alias (person)])
        (check-pred table-alias? alias)
        (check-eq? (table-alias-table alias) person)))
    
    (test-case "resolve column alias"
      (let ([alias (person id)])
        (check-pred column-alias? alias)
        (check-eq? (column-alias-column alias) (table-column person 'id))
        (check-eq? (table-alias-table (column-alias-table-alias alias)) person)))
    
    (test-case "resolve constraint alias"
      (let ([con (table-constraint person 'primary-key)])
        (check-pred primary-key? con)
        (check-eq? (constraint-name con) 'primary-key)
        (check-equal? (primary-key-columns con)
                      (list (table-column person 'id)))))))
