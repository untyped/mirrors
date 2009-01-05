#lang scheme/unit

(require scheme/match
         (planet untyped/unlib:3/time)
         (planet untyped/unlib:3/profile)
         "../base.ss"
         "../era/era.ss"
         "../sql/sql-struct.ss"
         "sql-data-sig.ss"
         "sql-name-sig.ss"
         "sql-query-helpers-sig.ss"
         "sql-query-sig.ss")

(import sql-data^
        sql-name^)

(export sql-query^
        sql-query-helpers^)

; Public -----------------------------------------

; query -> string
(define (query-sql query)
  (let ([out (open-output-string)])
    (display-query query out)
    (display ";" out)
    (get-output-string out)))

; Helpers ----------------------------------------

; query output-port -> void
(define (display-query query out)
  (define what      (query-what     query))
  (define distinct  (query-distinct query))
  (define from      (query-from     query))
  (define where     (query-where    query))
  (define group     (query-group    query))
  (define order     (query-order    query))
  (define having    (query-having   query))
  (define limit     (query-limit    query))
  (define offset    (query-offset   query))
  ;(define local     (query-local-columns query))
  (define imported  (query-imported-columns query))
  (define imported* (append what imported))
  (display "SELECT " out)
  (when distinct
    (display-distinct distinct imported* out)) 
  (display-what what imported out)
  (display " FROM " out)
  (display-from from imported out)
  (when where
    (display " WHERE " out)
    (display-expression where imported out))
  (unless (null? group)
    (display " GROUP BY " out)
    (display-group group imported* out))
  (when having
    (display " HAVING " out)
    (display-expression imported* out))
  (unless (null? order)
    (display " ORDER BY " out)
    (display-order order imported* out))
  (when limit
    (display " LIMIT " out)
    (display limit out))
  (when offset
    (display " OFFSET " out)
    (display offset out)))

; DISTINCT clause --------------------------------

; (U expression boolean) (listof column) output-port -> void
(define (display-distinct distinct imported out)
  (if (null? distinct)
      (begin (display "DISTINCT " out))
      (begin (display "DISTINCT ON " out)
             (display-expression (car distinct) imported out)
             (let loop ([distinct (cdr distinct)])
               (unless (null? distinct)
                 (display ", " out)
                 (display-expression (car distinct) imported out)
                 (loop (cdr distinct))))
             (display " " out))))

; WHAT clause ------------------------------------

; (listof column) (listof column) output-port -> void
;
; Displays an SQL fragment for a "WHAT" statement (the bit between "SELECT" and "FROM").
; Doesn't include the words "SELECT" or "FROM".
;
; The "declared" argument is a list of fields that have been declared in subqueries.
(define (display-what what imported out)
  (let loop ([what what] [first? #t])
    (unless (null? what)
      (unless first?
        (display ", " out))
      (display-what-item (car what) imported out)
      (loop (cdr what) #f))))

; column (listof column) output-port -> void
(define (display-what-item column imported out)
  (unless (member column imported)
    (if (attribute-alias? column)
        (display-expression column imported out)
        (display-expression (expression-alias-value column) imported out))
    (display " AS " out))
  (display (escape-name (column-name column)) out))

; FROM clause ------------------------------------

; source (listof column) output-port -> void
;
; Displays an SQL fragment for a FROM statement. Doesn't include the word "FROM".
(define (display-from from imported out)
  (cond [(join? from)         (display-from/join from imported out)]
        [(entity-alias? from) (display-from/entity from out)]
        [(query-alias? from)  (display-from/query from out)]
        [else          (raise-exn exn:fail:contract
                         (format "Expected source, received ~a" from))]))

; join (listof column) output-port -> void
(define (display-from/join the-join imported out)
  (match the-join
    [(struct join (op left right on))
     (display "(" out)
     (display-from left imported out)
     (cond [(eq? op 'inner) (display " INNER JOIN " out)]
           [(eq? op 'left)  (display " LEFT JOIN "  out)]
           [(eq? op 'right) (display " RIGHT JOIN " out)]
           [(eq? op 'outer) (display " CROSS JOIN " out)]
           [else            (raise-exn exn:fail:contract
                              (format "Join operator: expected (U 'inner 'outer 'left 'right), received ~a" op))])
     (display-from right imported out)
     (unless (eq? op 'outer)
       (display " ON " out)
       (display-expression on imported out))
     (display ")" out)]))

; entity-alias output-port -> void
(define (display-from/entity alias out)
  (match alias
    [(struct entity-alias (id entity))
     (display (escape-name (entity-table-name entity)) out)
     (display " AS " out)
     (display (escape-name id) out)]))

; query-alias output-port -> void
(define (display-from/query alias out)
  (match alias
    [(struct query-alias (id query))
     (display "(" out)
     (display-query query out)
     (display ") AS " out)
     (display (escape-name id) out)]))

; GROUP clause -----------------------------------

; (listof column) (listof column) output-port -> void
;
; Displays an SQL fragment for an GROUP BY statement. Doesn't include the words "GROUP BY".
(define (display-group group imported out)
  (let loop ([group group] [first? #t])
    (unless (null? group)
      (unless first?
        (display ", " out))
      (display-expression (car group) imported out)
      (loop (cdr group) #f))))

; ORDER clause -----------------------------------

; (listof order) (listof column) output-port -> void
;
; Displays an SQL fragment for an ORDER BY statement. Doesn't include the word "ORDER BY".
(define (display-order order imported out)
  (let loop ([order order] [first? #t])
    (unless (null? order)
      (unless first?
        (display ", " out))
      (display-order-item (car order) imported out)
      (loop (cdr order) #f))))

; order (listof column) output-port -> void
(define (display-order-item the-order imported out)
  (match the-order
    [(struct order (expr dir))
     (display-expression expr imported out)
     (if (eq? dir 'asc)
         (display " ASC" out)
         (display " DESC" out))]))

; Helpers ----------------------------------------

; expression (listof column) output-port -> void
(define (display-expression expr imported out)
  (cond [(literal? expr)          (display-expression/literal          expr out)]
        [(aggregate? expr)        (display-expression/aggregate        expr imported out)]
        [(function? expr)         (display-expression/function         expr imported out)]
        [(attribute-alias? expr)  (display-expression/attribute-alias  expr imported out)]
        [(expression-alias? expr) (display-expression/expression-alias expr imported out)]
        [(source-alias? expr)     (display-expression/source-alias     expr out)]
        [else                     (raise-exn exn:fail:contract (format "Expected expression, received ~a" expr))]))

; literal output-port -> void
(define (display-expression/literal lit out)
  (match lit
    [(struct literal (type val))
     (display (escape-value type val) out)]))

; expression (listof column) output-port -> void
(define (display-expression/aggregate agg imported out)
  (match agg
    [(struct aggregate (type op args))
     (case op
       [(count)   (display "COUNT(" out)]
       [(count*)  (display "COUNT(" out)]
       [(max)     (display "MAX(" out)]
       [(min)     (display "MIN(" out)]
       [(average) (display "AVERAGE(" out)]
       [else      (raise-exn exn:fail:contract (format "Unknown aggregate operator: ~a" op))])
     (case op
       [(count*)  (if (null? args)
                      (begin (display "*)" out))
                      (begin (display-expression (car args) imported out)
                             (display ".*)" out)))]
       [else      (display-expression (car args) imported out)
                  (display ")" out)])]))

; function (listof column) output-port -> void
(define (display-expression/function func imported out)
  (match func
    [(struct function (type op args))
     (case op
       [(and)                (display-expression/infix  args " AND " imported out)]
       [(or)                 (display-expression/infix  args " OR " imported out)]
       [(not)                (display-expression/outfix args "NOT " #f imported out)]
       [(+)                  (display-expression/infix  args " + " imported out)]
       [(-)                  (display-expression/infix  args " - " imported out)]
       [(*)                  (display-expression/infix  args " * " imported out)]
       [(/)                  (display-expression/infix  args " / " imported out)]
       [(abs)                (display-expression/outfix args "abs(" ")" imported out)]
       [(floor)              (display-expression/outfix args "floor(" ")" imported out)]
       [(ceiling)            (display-expression/outfix args "ceiling(" ")" imported out)]
       [(round)              (display-expression/outfix args "round(" ")" imported out)]
       [(=)                  (display-expression/infix  args " = " imported out)]
       [(<>)                 (display-expression/infix  args " <> " imported out)]
       [(<)                  (display-expression/infix  args " < " imported out)]
       [(>)                  (display-expression/infix  args " > " imported out)]
       [(<=)                 (display-expression/infix  args " <= " imported out)]
       [(>=)                 (display-expression/infix  args " >= " imported out)]
       [(like)               (display-expression/infix  args " LIKE " imported out)]
       [(regexp-match)       (display-expression/infix  args " ~ " imported out)]
       [(regexp-match-ci)    (display-expression/infix  args " ~* " imported out)]
       [(regexp-replace)     (display-expression/outfix args "regexp_replace(" ")" imported out)]
       [(regexp-replace-ci)  (display-expression/outfix args "regexp_replace(" ", 'i')" imported out)]
       [(regexp-replace*)    (display-expression/outfix args "regexp_replace(" ", 'g')" imported out)]
       [(regexp-replace*-ci) (display-expression/outfix args "regexp_replace(" ", 'gi')" imported out)]
       [(string-append)      (display-expression/infix  args " || " imported out)]
       [(string-replace)     (display-expression/outfix args "replace(" ")" imported out)]
       [(null?)              (display-expression/outfix args #f " IS NULL" imported out)]
       [(coalesce)           (display-expression/outfix args "coalesce(" ")" imported out)]
       [(->string)           (display-expression/outfix args "to_char(" ")" imported out)]
       [(->symbol)           (display-expression/outfix args "to_char(" ")" imported out)]
       [(in)                 (display-expression/in     (car args) (cadr args) imported out)]
       [(if)                 (display-expression/if     (car args) (cadr args) (caddr args) imported out)]
       [else                 (raise-exn exn:fail:contract (format "Unknown function operator: ~a" op))])]))

; (listof expression) string (listof column) output-port -> void
(define (display-expression/infix args delim imported out)
  (display "(" out)
  (let loop ([args args] [first? #t])
    (unless (null? args)
      (unless first?
        (display delim out))
      (display-expression (car args) imported out)
      (loop (cdr args) #f)))
  (display ")" out))

; (listof expression) string string (listof column) output-port -> void
(define (display-expression/outfix args prefix suffix imported out)
  (display "(" out)
  (when prefix
    (display prefix out))
  (let loop ([args args])
    (unless (null? args)
      (display-expression (car args) imported out)
      (unless (null? (cdr args))
        (display ", " out))
      (loop (cdr args))))
  (when suffix
    (display suffix out))
  (display ")" out))

; expression (U query (listof expression)) (listof column) output-port -> void
(define (display-expression/in item items imported out)
  (match items
    [(? null?)
     (display "(false)" out)]
    [(? pair?)
     (display "(" out)
     (display-expression item imported out)
     (display " IN (" out)
     (let loop ([items items] [first? #t])
       (unless (null? items)
         (unless first?
           (display ", " out))
         (display-expression (car items) imported out)
         (loop (cdr items) #f)))
     (display "))" out)]
    [(? query?) 
     (display "(" out)
     (display-expression item imported out)
     (display " IN (" out)
     (display-query items out)
     (display "))" out)]
    [(? query-alias?) 
     (display "(" out)
     (display-expression item imported out)
     (display " IN " out)
     (display-expression items imported out)
     (display ")" out)]))

; expression expression expression (listof column) output-port -> void
(define (display-expression/if test pos neg imported out)
  (display "(CASE" out)
  (let loop ([test test] [pos pos] [neg neg])
    (display " WHEN " out)
    (display-expression test imported out)
    (display " THEN " out)
    (display-expression pos imported out)
    (if (and (function? neg) (eq? (function-op neg) 'if))
        (begin (apply loop (function-args neg)))
        (begin (display " ELSE " out)
               (display-expression neg imported out))))
  (display " END)" out))

; attribute-alias (listof column) output-port -> void
(define (display-expression/attribute-alias alias imported out)
  (if (member alias imported)
      (begin (display (escape-name (column-name alias)) out))
      (begin (display (escape-name (source-alias-name (attribute-alias-entity alias))) out)
             (display "." out)
             (display (escape-name (attribute-column-name (attribute-alias-attribute alias))) out))))

; attribute-alias (listof column) output-port -> void
(define (display-expression/expression-alias alias imported out)
  (if (member alias imported)
      (display (escape-name (column-name alias)) out)
      (raise-exn exn:fail:contract
        (format "Expression alias not in scope: ~s" alias))))

; source-alias output-port -> void
(define (display-expression/source-alias alias out)
  (display (escape-name (source-alias-name alias)) out))
