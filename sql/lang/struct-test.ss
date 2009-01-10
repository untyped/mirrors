#lang scheme/base

(require srfi/19
         srfi/26
         "../../test-base.ss"
         "define-table.ss"
         "struct.ss"
         "type.ss")

; Test data --------------------------------------

;  symbol
;  (table -> (listof column))
;  (table (listof column) -> (listof constraint))
; ->
;  table
;  column ...
(define (make-table+columns name make-columns make-constraints)
  (define table
    (make-table name make-columns make-constraints))
  (apply values table (table-columns table)))

; table
(define person
  (make-table 'person))

; column ...
(define-values (person-id person-name person-age)
  (values (add-table-column! person 'id   (make-integer-type)   #f #f)
          (add-table-column! person 'name (make-string-type 32) #t #f)
          (add-table-column! person 'age  (make-integer-type)   #f #f)))

; primary-key
(define person-primary-key
  (add-table-primary-key! person 'primary-key person-id person-name))

; Tests ------------------------------------------

; test-suite
(define struct-tests
  (test-suite "struct.ss"
    
    ; Constructors and accessors
    
    (test-case "make-table"
      (let ([table (make-table 'name)])
        (check-pred table? table)
        (check-pred null? (table-columns table))
        (check-pred null? (table-constraints table))))
    
    (test-case "add-table-column!"
      (let* ([table (make-table 'name)]
             [col1  (add-table-column! table 'col1 (make-integer-type))]
             [col2  (add-table-column! table 'col2 (make-integer-type) #f)]
             [col3  (add-table-column! table 'col3 (make-integer-type) #f #f)])
        (check-eq? (column-table col1) table)
        (check-eq? (column-table col2) table)
        (check-eq? (column-table col3) table)
        (check-eq? (column-name col1) 'col1)
        (check-eq? (column-name col2) 'col2)
        (check-eq? (column-name col3) 'col3)
        (check-equal? (table-columns table) (list col1 col2 col3))))
    
    (test-case "add-table-primary-key!"
      (let* ([table (make-table 'name)]
             [col1  (add-table-column! table 'col1 (make-integer-type))]
             [col2  (add-table-column! table 'col2 (make-integer-type) #f)]
             [con   (add-table-primary-key! table 'con col1 col2)])
        (check-pred primary-key? con)
        (check-eq? (constraint-table con) table)
        (check-equal? (constraint-name con) 'con)
        (check-equal? (primary-key-columns con) (list col1 col2))
        (check-equal? (table-constraints table) (list con))))
    
    (test-case "add-table-foreign-key!"
      (let* ([table1 (make-table 'name1)]
             [col1   (add-table-column! table1 'col1 (make-integer-type))]
             [table2 (make-table 'name2)]
             [col2   (add-table-column! table2 'col2 (make-integer-type))]
             [con    (add-table-foreign-key! table1 'con col1 col2)])
        (check-pred foreign-key? con)
        (check-eq? (constraint-table con) table1)
        (check-equal? (constraint-name con) 'con)
        (check-equal? (foreign-key-local con) col1)
        (check-equal? (foreign-key-foreign con) col2)
        (check-equal? (table-constraints table1) (list con))))
    
    (test-case "add-table-unique-constraint!"
      (let* ([table (make-table 'name)]
             [col1  (add-table-column! table 'col1 (make-integer-type))]
             [col2  (add-table-column! table 'col2 (make-integer-type) #f)]
             [con   (add-table-unique-constraint! table 'con col1 col2)])
        (check-pred unique-constraint? con)
        (check-eq? (constraint-table con) table)
        (check-equal? (constraint-name con) 'con)
        (check-equal? (unique-constraint-columns con) (list col1 col2))
        (check-equal? (table-constraints table) (list con))))
    
    (test-case "add-table-check-constraint!"
      (let* ([table       (make-table 'name)]
             [col1        (add-table-column! table 'col1 (make-integer-type))]
             [col2        (add-table-column! table 'col2 (make-integer-type) #f)]
             [table-alias (make-table-alias 'table table)]
             [col1-alias  (make-column-alias table-alias col1)]
             [col2-alias  (make-column-alias table-alias col2)]
             [expr        (make-function (make-boolean-type) '> 
                                         (list col1-alias col2-alias))]
             [con         (add-table-check-constraint! table 'con expr)])
        (check-pred check-constraint? con)
        (check-eq? (constraint-table con) table)
        (check-equal? (constraint-name con) 'con)
        (check-equal? (check-constraint-expression con) expr)
        (check-equal? (table-constraints table) (list con))))
    
    (test-case "multiple constraints"
      (let* ([table       (make-table 'name)]
             [col1        (add-table-column! table 'col1 (make-integer-type))]
             [col2        (add-table-column! table 'col2 (make-integer-type) #f)]
             [con1        (add-table-unique-constraint! table 'con col1 col2)]
             [con2        (add-table-unique-constraint! table 'con col2 col1)])
        (check-equal? (table-constraints table) (list con1 con2))))))

; Provide statements -----------------------------

(provide struct-tests)

