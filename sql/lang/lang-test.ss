#lang scheme/base

(require mzlib/etc
         (only-in srfi/1 unzip2)
         srfi/19
         srfi/26
         "../../test-base.ss"
         "define-table.ss"
         (prefix-in sql: "lang.ss")
         "struct.ss"
         "type.ss")

; Test data --------------------------------------

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

(define test-query
  (make-select null #f (person) #f null null #f #f #f null null))

; Tests ------------------------------------------

; test-suite
(define lang-tests
  (test-suite "lang.ss"
    
    (test-case "alias"
      (check-equal? (sql:alias 'p person) (make-table-alias 'p person))
      (check-equal? (sql:alias 'q test-query) (make-select-alias 'q test-query))
      (check-equal? (sql:alias 'l (make-literal 1)) (make-expression-alias 'l (make-literal 1)))
      (check-exn exn:fail:contract?
        (lambda ()
          (sql:alias 'm (make-expression-alias 'l (make-literal 1)))))
      (check-equal? (sql:alias (sql:alias 'p person) (table-column person 'name))
                    (make-column-alias (make-table-alias 'p person) (table-column person 'name)))
      (check-exn exn:fail:contract?
        (lambda ()
          (sql:alias 'p-name person (table-column employer 'name)))))

    (test-case "expressions : function arities and self-quoting literals"
      (define-values (sql:ops args)
        (unzip2 (list (list sql:and                (list #t #f #t))
                      (list sql:or                 (list #t #f #t))
                      (list sql:not                (list #t))
                      (list sql:+                  (list 1 2.5 3))
                      (list sql:-                  (list 1 2.5 3))
                      (list sql:*                  (list 1 2.5 3))
                      (list sql:/                  (list 1 2.5))
                      (list sql:string-append      (list 'a "b" 'c))
                      (list sql:string-replace     (list "mat" "a" "e"))
                      (list sql:=                  (list #t 'a))
                      (list sql:<>                 (list #t 'a))
                      (list sql:<                  (list 1 2.5))
                      (list sql:>                  (list 'a "b"))
                      (list sql:<=                 (list (current-time time-tai) (current-time time-utc)))
                      (list sql:>=                 (list (current-time time-tai) (current-time time-utc)))
                      (list sql:like               (list "a" 'b))
                      (list sql:regexp-match       (list "a" 'b))
                      (list sql:regexp-match-ci    (list "a" 'b))
                      (list sql:regexp-replace     (list "a" 'b "c"))
                      (list sql:regexp-replace-ci  (list "a" 'b "c"))
                      (list sql:regexp-replace*    (list "a" 'b "c"))
                      (list sql:regexp-replace*-ci (list "a" 'b "c"))
                      (list sql:null?              (list #t)))))
      
      (for-each (lambda (sql:op args)
                  (define literals (map sql:literal args))
                  (check-equal? (apply sql:op args)
                                (apply sql:op literals)
                                (format "~s" (cons sql:op args))))
                sql:ops
                args))

    #|
    (test-case "sql:in"
      (begin-with-definitions
        
        (define-alias p person)
        
        (check-not-exn
          (cut sql:in 123 '(123 234 345))
          "valid list")
        
        (check-exn exn:fail:contract?
          (cut sql:in 123 '(#f 234 345))
          "invalid list (list items of different types)")
        
        (check-exn exn:fail:contract?
          (cut sql:in "123" '(123 234 345))
          "invalid list (arguments of different types)")
        
        (check-not-exn
          (cut sql:in 123 (sql:select #:what p-id #:from p))
          "valid subselect")
        
        (check-exn exn:fail:contract?
          (cut sql:in 123 (sql:select #:from p))
          "invalid subselect (incorrect arity)")
        
        (check-exn exn:fail:contract?
          (cut sql:in "123" (sql:select #:what p-id #:from p))
          "invalid subselect (arguments of different types)")))
    
    (test-case "sql:if and sql:cond"
      (check-equal? (sql:cond [#t "a"] [#f "b"] [else "c"])
                    (sql:if #t "a" (sql:if #f "b" "c"))
                    "else")
      (check-equal? (sql:cond [#t "a"] [#f "b"])
                    (sql:if #t "a" (sql:if #f "b"))
                    "no else")
      (check-true (integer-type? (expression-type (sql:cond [#t 1] [#f 2.0]))) "single type cond")
      (check-true (real-type? (expression-type (sql:cond [#t 1] [#f 2.1]))) "multiple type cond"))
    
    (test-case "joins: terms in join condition must be part of join operands"
      (begin-with-definitions
        
        (define-alias p1 person)
        (define-alias p2 person)
        (define-alias p3 person)
        
        (check-not-exn 
          (cut sql:inner p1 p2 (sql:= p1-name p2-name))
          "valid")
        
        (check-exn exn:fail:contract?
          (cut sql:inner p1 p3 (sql:= p1-name p2-name))
          "invalid")
        
        (check-exn exn:fail:contract?
          (cut sql:inner p1 p3 (sql:= p1-name p2-name))
          "tricky")))
    
    (test-case "select : single-item versus multi-item"
      (begin-with-definitions
        
        (define-alias p1 person)
        (define-alias p2 person)
        
        (define select1 (sql:select #:what (list p1-name) #:from p1))
        (define select2 (sql:select #:what p1-name #:from p1))
        
        (check-equal? (select-what select1) (select-what select2) "what")
        (check-equal? (select-extract-info select1) (list (select-extract-info select2)) "expand-info")))
    
    (test-case "select : no #:what"
      (begin-with-definitions
        
        (define p1 (sql:alias 'p1 person))
        (define p2 (sql:alias 'p2 table:pet))
        
        (define select1 (sql:select #:from p1))
        
        (check-equal? (select-extract-info (sql:select #:from p1))
                      person
                      "table")
        
        (check-equal? (select-extract-info (sql:select #:from (sql:outer p1 p2)))
                      (list person table:pet)
                      "join")))
    
    (test-case "select : queries in #:from get quoted"
      (begin-with-definitions
        
        (define p1 (sql:alias 'p1 person))
        
        (check-pred select-alias?
                    (select-from (sql:select #:from (sql:select #:from p1)))
                    "select")))
    
    (test-case "select : members of #:what must be defined in #:from"
      (begin-with-definitions 
        
        (define-alias p1 person)
        (define-alias p2 person)
        (define-alias p1-count (sql:count* p1))
        
        (check-not-exn
          (cut sql:select #:what p1-name #:from p1)
          "correct table")
        
        (check-not-exn
          (cut sql:select #:what p1-name #:from (sql:outer p1 p2))
          "join")
        
        (check-exn exn:fail:contract?
          (cut sql:select #:what p1-name #:from p2)
          "incorrect table")
        
        (check-not-exn
          (cut sql:select #:what p1-name #:from (sql:select #:what p1-name #:from p1))
          "subselect with correct #:what")
        
        (check-exn exn:fail:contract?
          (cut sql:select #:what p1-name #:from (sql:select #:what p1-id #:from p1))
          "subselect with incorrect #:what")
        
        (check-not-exn
          (cut sql:select #:what p1-count #:from (sql:select #:what p1-count #:from p1))
          "redeclared expression")))
    
    (test-case "select : members of #:distinct must be defined in #:from"
      (begin-with-definitions 
        
        (define-alias p1 person)
        (define-alias p2 person)
        (define-alias p1-count (sql:count* p1))
        
        (check-not-exn
          (cut sql:select #:distinct #t #:from p1)
          "no table")
        
        (check-not-exn
          (cut sql:select #:distinct (sql:= p1-name "x") #:from p1)
          "single table")
        
        (check-not-exn
          (cut sql:select #:distinct (sql:= p1-name p2-name) #:from (sql:outer p1 p2))
          "join")
        
        (check-exn exn:fail:contract?
          (cut sql:select #:distinct (sql:= p1-name "x") #:from p2)
          "incorrect table")
        
        (check-not-exn
          (cut sql:select #:distinct (sql:= p1-name "x") #:from (sql:select #:what p1-name #:from p1))
          "subselect with correct #:what")
        
        (check-exn exn:fail:contract?
          (cut sql:select #:distinct (sql:= p1-name "x") #:from (sql:select #:what p1-id #:from p1))
          "subselect with incorrect #:what")
        
        (check-not-exn
          (cut sql:select #:distinct (sql:= p1-count 1) #:from (sql:select #:what p1-count #:from p1))
          "redeclared expression")))
    
    (test-case "select : members of #:where must be defined in #:from"
      (begin-with-definitions
        
        (define-alias p1 person)
        (define-alias p2 person)
        
        (define p1-max-id (sql:alias 'p1-max-id (sql:max p1-id)))
        
        (check-not-exn
          (cut sql:select #:what p1-id #:from p1 #:where (sql:= p1-name "Dave"))
          "table")
        
        (check-not-exn
          (cut sql:select #:what p1-id #:from (sql:outer p1 p2) #:where (sql:= p1-name "Dave"))
          "join")
        
        (check-not-exn
          (cut sql:select #:what p1-id #:from (sql:select #:from p1) #:where (sql:= p1-name "Dave"))
          "subselect")
        
        (check-not-exn
          (cut sql:select 
               #:what  p2-id
               #:from  (sql:outer (sql:select #:what p1-max-id #:from p1) p2)
               #:where (sql:= p2-id p1-max-id))
          "expression alias")
        
        (check-exn exn:fail:contract?
          (cut sql:select #:from p2 #:where (sql:= p1-name "Dave"))
          "incorrect table")))
    
    (test-case "select : members of #:order must be defined in #:what or #:from"
      (begin-with-definitions 
        
        (define-alias p1 person)
        (define-alias p2 person)
        
        (define-alias expr1 (sql:+ p1-id 1))
        
        (check-not-exn
          (cut sql:select #:what p1-id #:from p1 #:order (list (sql:asc p1-name)))
          "table")
        
        (check-exn exn:fail:contract?
          (cut sql:select #:what p1-id #:from p1 #:order (list (sql:asc p2-name)))
          "incorrect table")
        
        (check-not-exn
          (cut sql:select
               #:what  p1-id
               #:from  (sql:outer p1 p2)
               #:order (list (sql:asc p1-name)))
          "join")
        
        (check-not-exn
          (cut sql:select
               #:what  p1-id
               #:from  (sql:select #:from p1)
               #:order (list (sql:asc p1-name)))
          "subselect")
        
        (check-exn exn:fail:contract?
          (cut sql:select
               #:what  p2-name
               #:from  (sql:select #:from p2)
               #:order (list (sql:asc p1-name)))
          "incorrect subselect")
        
        (check-not-exn
          (cut sql:select
               #:what  (list p1-id expr1)
               #:from  (sql:select #:from (sql:outer p1 p2))
               #:order (list (sql:asc expr1) (sql:desc p1-id) (sql:asc p2-id)))
          "expressions and columns in #:what")))
    
    (test-case "select : extract-info"
      (begin-with-definitions
        
        (define-alias p1 person)
        (define-alias p2 person)
        
        (define p1-count-id   (sql:count   p1-id))
        (define p1-count      (sql:count*  p1))
        (define p1-max-id     (sql:max     p1-id))
        (define p1-min-id     (sql:min     p1-id))
        (define p1-average-id (sql:average p1-id))
        
        (check-equal? (select-extract-info (sql:select #:what p1-id #:from p1))
                      type:integer
                      "single column")
        
        (check-equal? (select-extract-info (sql:select #:what (list p1-id p1-revision p1-name) #:from p1))
                      (list type:id type:revision type:string)
                      "list of columns")
        
        (check-equal? (select-extract-info (sql:select #:from p1))
                      person
                      "single table")
        
        (check-equal? (select-extract-info (sql:select #:from (sql:outer p1 p2)))
                      (list person person)
                      "multiple entities")
        
        (check-equal? (select-extract-info (sql:select #:what (list p1-count-id
                                                                    p1-count
                                                                    p1-max-id
                                                                    p1-min-id
                                                                    p1-average-id)
                                                       #:from p1))
                      (list type:integer type:integer type:integer type:integer type:real)
                      "aggregates")
        
        (check-equal? (select-extract-info (sql:select #:what (list (sql:alias 'column1 (sql:+ p1-id p1-revision))
                                                                    (sql:alias 'column2 (sql:+ p1-count-id 1.5))
                                                                    (sql:alias 'column3 (sql:string-append p1-name " rocks!"))
                                                                    (sql:alias 'column4 (sql:> p1-id 123)))
                                                       #:from p1))
                      (list type:integer type:real type:string type:boolean)
                      "expressions")))
    |#
    ))

; Provide statements -----------------------------

(provide lang-tests)

