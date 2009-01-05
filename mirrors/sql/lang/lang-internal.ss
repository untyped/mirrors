#lang scheme/base

(require (for-syntax scheme/base)
         (only-in srfi/1 append-map)
         (planet untyped/unlib:3/symbol)
         "../../base.ss"
         "struct.ss"
         "type.ss")

; Extract info and automatic #:what arguments ----

; relation -> relation-list
;
; where relation-list : (listof relation)
(define relation->relations
  (match-lambda
    [(? join? join)          (append (relation->relations (join-left join))
                                     (relation->relations (join-right join)))]
    [(? select-alias? alias) (list alias)]
    [(? table-alias? alias)  (list alias)]))

; relation -> column-list column-list
;
; where column-list : (listof column)
;   and column      : (U expression-alias column-alias)
(define relation->columns
  (match-lambda
    [(? join? join)          
     (define-values (left-local left-imported)
       (relation->columns (join-left join)))
     (define-values (right-local right-imported)
       (relation->columns (join-right join)))
     (values (append left-local right-local)
             (append left-imported right-imported))]
    [(? select-alias? alias)  
     (values null (relation-attributes alias))]
    [(? table-alias? alias)
     (values (relation-attributes alias) null)]))

; relation -> (opt-listof (U expression table-alias select-alias))
(define (make-default-what-argument from)
  (if (join? from)
      (relation->relations from)
      (car (relation->relations from))))

;   ((opt-listof (U expression table-alias select-alias))
; ->
;    (listof column)
;    (opt-listof (U table type)))
;
; where (opt-listof x) = (U x (listof x))
(define (expand-what-argument argument)
  (if (list? argument)
      (expand-what-list argument)
      (expand-what-item argument)))

;  (listof (U expression table-alias select-alias))
; -> 
;  (listof column)
(define (expand-what-list items)
  (append-map (lambda (item)
                (expand-what-item item))
              items))

;  (U expression table-alias select-alias)
; -> 
;  (listof column)
(define expand-what-item
  (match-lambda
    [(? column-alias? item)     (list item)]
    [(? expression-alias? item) (list item)]
    [(? expression? expr)       (list (make-expression-alias (gensym 'expr) expr))]
    [(? relation? alias)        (relation-attributes alias)]
    [(? select-alias? alias)    (relation-attributes alias)]))

; (listof (U expression table-alias select-alias)) -> (listof column)
(define (expand-group-argument group)
  (append-map expand-group-item group))

; (U expression table-alias group-alias) -> (listof column)
(define expand-group-item
  (match-lambda
    [(? column? column)  (list column)]
    [(? relation? alias) (relation-attributes alias)]))

; (listof expression) relation-list column-list -> void
(define (check-what-clause what relations columns)
  ; -> expression void
  (define (check-item item)
    (cond [(column-alias? item)
           (check-attribute-in-scope 'what-clause item columns)]
          [(expression-alias? item) 
           (with-handlers ([exn? (lambda _ (check-expression 'what-clause (expression-alias-value item) relations columns))])
             (check-expression-in-scope 'what-clause item columns))]
          [else (raise-type-error 'what-clause "(U column-alias expression-alias)" item)]))
  (if (list? what)
      (for-each check-item what)
      (check-item what)))

; (U (listof expression) #f) relation-list column-list -> void
(define (check-distinct-clause distinct relations columns)
  (when distinct
    (for-each (cut check-expression 'distinct-clause <> relations columns)
              distinct)))

; (U expression #f) relation-list column-list -> void
(define (check-where-clause where relations columns)
  (when where
    (check-expression 'where-clause where relations columns)))

; (listof expression) relation-list column-list -> void
(define (check-group-clause group relations columns)
  (for-each (cut check-expression 'group-clause <> relations columns) group))

; (listof order) relation-list column-list -> void
(define (check-order-clause order relations columns)
  (for-each (cut check-expression 'order-clause <> relations columns)
            (map order-expression order)))

; (U expression #f) relation-list column-list -> void
(define (check-having-clause having relations columns)
  (when having
    (check-expression 'having-clause having relations columns)))

; Expression predicates --------------------------

; type ... -> (any -> boolean)
(define (make-expression-predicate . types)
  (match-lambda
    [(struct expression (type))
     (ormap (cut type-compatible? type <>) types)]
    [_ #f]))

; any -> boolean
(define boolean-expression?
  (make-expression-predicate (make-boolean-type)))

; any -> boolean
(define integer-expression?
  (make-expression-predicate (make-integer-type)))

; any -> boolean
(define real-expression?
  (make-expression-predicate (make-real-type)))

; any -> boolean
(define numeric-expression?
  (make-expression-predicate (make-integer-type)
                             (make-real-type )))

; any -> boolean
(define string-expression?
  (make-expression-predicate (make-string-type #f)))

; any -> boolean
(define symbol-expression?
  (make-expression-predicate (make-symbol-type #f)))

; any -> boolean
(define character-expression?
  (make-expression-predicate (make-string-type #f)
                             (make-symbol-type #f)))

; any -> boolean
(define time-utc-expression?
  (make-expression-predicate (make-time-utc-type)))

; any -> boolean
(define time-tai-expression?
  (make-expression-predicate (make-time-tai-type)))

; any -> boolean
(define temporal-expression?
  (make-expression-predicate (make-time-utc-type)
                             (make-time-tai-type)))

; define-function syntax -------------------------

(define-syntax (define-function stx)
  (define (remove-prefix sym)
    (let ([match (regexp-match #rx"^sql:(.*)$" (symbol->string sym))])
      (if match
          (string->symbol (cadr match))
          (error "define-function: identifier must have an 'sql:' prefix: " sym))))
  (syntax-case stx (else)
    [(_ (id arg ...) [rule type] ...)
     (identifier? #'id)
     (with-syntax ([plain-id           (remove-prefix (syntax->datum #'id))]
                   [(arg-contract ...) (map (lambda _ #'quotable?) (syntax->list #'(arg ...)))])
       #'(begin (define (id arg ...)
                  (let ([arg (sql-lift arg)] ...)
                    (make-function (cond [rule type] ...
                                         [else (error "~a not defined for the types: "
                                                      'id
                                                      (map expression-type (list arg ...)))])
                                   'plain-id
                                   (list arg ...))))
                (provide/contract [rename id plain-id (-> arg-contract ... function?)])))]
    [(_ (id . args) [rule type] ...)
     (identifier? #'id)
     (with-syntax ([plain-id (remove-prefix (syntax->datum #'id))])
       #'(begin (define (id . args)
                  (let ([args (map sql-lift args)])
                    (make-function (cond [rule type] ...
                                         [else (error (format "~a not defined for the types: "
                                                              'id
                                                              (map expression-type args)))])
                                   'plain-id
                                   args)))
                (provide/contract [rename id plain-id (->* () () #:rest (listof quotable?) function?)])))]))

; Helpers ----------------------------------------

; (U x (listof x)) -> (listof x)
(define (listify item)
  (if (or (pair? item) (null? item))
      item
      (list item)))

; relation-list -> void
(define (check-repeated-relations relations)
  ; void
  (let loop ([relations relations] [names (map relation-name relations)])
    (match relations
      [(list) (void)]
      [(list-rest curr rest)
       (when (memq (car names) (cdr names))
         (raise-exn exn:fail:contract
           (format "~a: relation selected more than once: ~a ~s"
                   'from-clause
                   (car names)
                   (car relations))))
       (loop (cdr relations) (cdr names))])))

; column-list -> void
(define (check-repeated-columns columns)
  ; void
  (let loop ([columns columns] [names (map column-name columns)])
    (match columns
      [(list) (void)]
      [(list-rest curr rest)
       (when (memq (car names) (cdr names))
         (raise-exn exn:fail:contract
           (format "~a: column selected more than once: ~a ~s"
                   'from-clause
                   (car names)
                   (car columns))))
       (loop (cdr columns) (cdr names))])))

; symbol column-alias column-list -> void
(define (check-attribute-in-scope name attr columns)
  ; Use member here as some attributes in columns are inferred from table-aliases:
  (unless (member attr columns)
    (raise-exn exn:fail:contract
      (format "~a: attribute not in scope: ~s" name attr))))

; symbol expression-alias column-list -> void
(define (check-expression-in-scope name expr columns)
  ; Use memq here as expressions have to be eq? to one another:
  (unless (memq expr columns)
    (raise-exn exn:fail:contract
      (format "~a: expression not in scope: ~s" name expr))))

; symbol relation relation-list -> void
(define (check-relation-in-scope name relation relations)
  (unless (memq relation relations)
    (raise-exn exn:fail:contract
      (format "~a: relation not in scope: ~s" name relation))))

; join -> void
(define (check-join j)
  (define relations (relation->relations j))
  (define columns (call-with-values (cut relation->columns j) append))
  (match j
    [(struct join (op left right on))
     (check-repeated-relations relations)
     (check-repeated-columns columns)
     (when on
       (check-expression op on relations columns)
       (check-no-aggregates op on))]))

; symbol column-alias relation-list column-list -> void
(define (check-expression name expr relations columns)
  (cond [(column-alias? expr)     (check-attribute-in-scope name expr columns)]
        [(expression-alias? expr) (check-expression-in-scope name expr columns)]
        [(function? expr)         (for-each (cut check-expression name <> relations columns)
                                            ; Skip select arguments to sql:in:
                                            (if (and (eq? (function-op expr) 'in) 
                                                     (select? (cadr (function-args expr))))
                                                (list (car (function-args expr)))
                                                (function-args expr)))]
        [(literal? expr)          (void)]
        ; Special cases for count* and in:
        [(relation? expr)         (check-relation-in-scope name expr relations)]
        [(list? expr)             (for-each (cut check-expression name <> relations columns) expr)]))

; symbol expression -> void
(define (check-no-aggregates name expr)
  (cond [(aggregate? expr) (error (format "~a: aggregates not allowed: ~s" name expr))]
        [(function? expr)  (for-each (cut check-no-aggregates name <>) (function-args expr))]
        ; Special case for in:
        [(list? expr)      (for-each (cut check-no-aggregates name <>) expr)]
        [else (void)]))

; Provide statements -----------------------------

; (opt-listof contract) -> contract
(define (opt-listof item/c)
  (or/c item/c (listof item/c)))

(provide check-join
         check-what-clause
         check-distinct-clause
         check-where-clause
         check-group-clause
         check-order-clause
         check-having-clause
         check-no-aggregates
         boolean-expression?
         integer-expression?
         real-expression?
         string-expression?
         symbol-expression?
         time-utc-expression?
         time-tai-expression?
         numeric-expression?
         character-expression?
         temporal-expression?
         define-function)

(provide/contract
 [relation->relations        (-> relation? (listof relation?))]
 [relation->columns          (-> relation? (values (listof column?) (listof column?)))]
 [make-default-what-argument (-> relation? (opt-listof relation?))]
 [expand-what-argument       (-> (opt-listof (or/c expression? relation?))
                                 (values (listof column?)
                                         (opt-listof (or/c table? type?))))]
 [expand-group-argument      (-> (opt-listof (or/c expression? relation?))
                                 (listof column?))])
