#lang scheme/base

(require "../../test-base.ss"
         "struct.ss"
         "type.ss")

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

; Provide statements -----------------------------

(provide/contract
 [person             table?]
 [person-id          column?]
 [person-name        column?]
 [person-age         column?]
 [person-primary-key primary-key?])