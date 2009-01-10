#lang scheme/base

(require scheme/class
         "../../test-base.ss"
         "type.ss")

; Tests -------------------------------------------

(define type-tests
  (test-suite "type.ss"
    
    (test-case "boolean-type"
      (define type1 (make-boolean-type))
      (define type2 (make-boolean-type))
      (define type3 (make-integer-type))
      
      (check-true (type-valid? type1 #t))
      (check-true (type-valid? type1 #f))
      (check-false (type-valid? type1 0))
      
      (check-true (type-null? type1 (void)))
      (check-false (type-null? type1 #t))
      (check-false (type-null? type1 #f))
      
      (check-equal? (type-null type1) (void))
      
      (check-true (type-compatible? type1 type2))
      (check-false (type-compatible? type1 type3)))
    
    (test-case "integer-type"
      (define type1 (make-integer-type))
      (define type2 (make-real-type))
      (define type3 (make-boolean-type))
      
      (check-true (type-valid? type1 1))
      (check-true (type-valid? type1 0))
      (check-true (type-valid? type1 -1))
      (check-true (type-valid? type1 #f))
      (check-false (type-valid? type1 #t))
      
      (check-true (type-null? type1 #f))
      (check-false (type-null? type1 -1))
      
      (check-equal? (type-null type1) #f)
      
      (check-true (type-compatible? type1 type2))
      (check-false (type-compatible? type1 type3)))
    
    (test-case "real-type"
      (define type1 (make-real-type))
      (define type2 (make-integer-type))
      (define type3 (make-boolean-type))
      
      (check-true (type-valid? type1 1.23))
      (check-true (type-valid? type1 0))
      (check-true (type-valid? type1 -1.23))
      (check-true (type-valid? type1 #f))
      (check-false (type-valid? type1 #t))
      
      (check-true (type-null? type1 #f))
      (check-false (type-null? type1 -1.23))
      
      (check-equal? (type-null type1) #f)
      
      (check-true (type-compatible? type1 type2))
      (check-false (type-compatible? type1 type3)))
    
    (test-case "string-type"
      (define type1 (make-string-type 3))
      (define type2 (make-string-type #f))
      (define type3 (make-symbol-type #f))
      (define type4 (make-boolean-type))
      
      (check-true (type-valid? type1 "str"))
      (check-true (type-valid? type1 #f))
      (check-false (type-valid? type1 'sym))
      (check-false (type-valid? type1 "str1"))
      (check-true (type-valid? type2 "str1"))
      
      (check-true (type-null? type1 #f))
      (check-false (type-null? type1 "str"))
      
      (check-equal? (type-null type1) #f)
      
      (check-true (type-compatible? type1 type2))
      (check-true (type-compatible? type1 type3))
      (check-false (type-compatible? type1 type4)))
    
    (test-case "symbol-type"
      (define type1 (make-symbol-type 3))
      (define type2 (make-symbol-type #f))
      (define type3 (make-string-type #f))
      (define type4 (make-boolean-type))
      
      (check-true (type-valid? type1 'sym))
      (check-true (type-valid? type1 #f))
      (check-false (type-valid? type1 "str"))
      (check-false (type-valid? type1 'sym1))
      (check-true (type-valid? type2 'sym1))
      
      (check-true (type-null? type1 #f))
      (check-false (type-null? type1 'sym))
      
      (check-equal? (type-null type1) #f)
      
      (check-true (type-compatible? type1 type2))
      (check-true (type-compatible? type1 type3))
      (check-false (type-compatible? type1 type4)))
    
    (test-case "time-utc-type"
      (define type1 (make-time-utc-type))
      (define type2 (make-time-tai-type))
      (define type3 (make-boolean-type))
      
      (check-true (type-valid? type1 (current-time time-utc)))
      (check-true (type-valid? type1 #f))
      (check-false (type-valid? type1 (current-time time-tai)))
      
      (check-true (type-null? type1 #f))
      (check-false (type-null? type1 (current-time time-utc)))
      
      (check-equal? (type-null type1) #f)
      
      (check-true (type-compatible? type1 type2))
      (check-false (type-compatible? type1 type3)))
    
    (test-case "time-tai-type"
      (define type1 (make-time-tai-type))
      (define type2 (make-time-utc-type))
      (define type3 (make-boolean-type))
      
      (check-true (type-valid? type1 (current-time time-tai)))
      (check-true (type-valid? type1 #f))
      (check-false (type-valid? type1 (current-time time-utc)))
      
      (check-true (type-null? type1 #f))
      (check-false (type-null? type1 (current-time time-tai)))
      
      (check-equal? (type-null type1) #f)
      
      (check-true (type-compatible? type1 type2))
      (check-false (type-compatible? type1 type3)))))

; Provide statements -----------------------------

(provide type-tests)