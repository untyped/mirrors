#lang scheme/base

(require (only-in scheme/list remove-duplicates)
         (planet untyped/unlib:3/time)
         (planet untyped/unlib:3/symbol)
         "../../base.ss"
         "struct-internal.ss"
         "type.ss")

; Constructors -----------------------------------

; There is a structure type called "create" with a field called "table".
; "create-table" isn't available as a procedure name, so in the interests
; of consistency we use "construct-" as a constructor name prefix.

; symbol -> table
(define (construct-table name)
  (make-table name null null))

; table symbol [boolean] [any] -> column
(define (add-table-column! table name type [not-null? #f] [default #f])
  (let ([ans (make-column table name type not-null? default)])
    (set-table-columns! table (append (table-columns table) (list ans)))
    ans))

; table symbol column ... -> primary-key
(define (add-table-primary-key! table name . cols)
  (when (ormap primary-key? (table-constraints table))
    (error "table already has a primary key" table))
  (let ([ans (make-primary-key table name cols)])
    (set-table-constraints! table (append (table-constraints table) (list ans)))
    ans))

; table symbol (U column column-alias) (U column column-alias) -> foreign-key
(define (add-table-foreign-key! table name local foreign)
  (define local-col
    (if (column? local)
        local
        (column-alias-column local)))
  (define foreign-col
    (if (column? foreign)
        foreign
        (column-alias-column foreign)))
  (unless (eq? table (column-table local))
    (error (format "local table does not contain column: ~s ~s" table local)))
  (when (eq? local foreign)
    (error (format "local and referenced columns are the same: ~s ~s" local foreign)))
  (let ([ans (make-foreign-key table name local-col foreign-col)])
    (set-table-constraints! table (append (table-constraints table) (list ans)))
    ans))

; table symbol column ... -> unique-constraint
(define (add-table-unique-constraint! table name . cols)
  (when (null? cols)
    (error "must supply one or more columns"))
  (unless (equal? cols (remove-duplicates cols eq?))
    (error "all columns must be unique"))
  (for ([col (in-list cols)])
    (unless (eq? table (column-table col))
      (error (format "table does not contain column: ~s ~s" table col))))
  (let ([ans (make-unique-constraint table name cols)])
    (set-table-constraints! table (append (table-constraints table) (list ans)))
    ans))

; symbol expression -> check-constraint
(define (add-table-check-constraint! table name expr) 
  (let ([ans (make-check-constraint table name expr)])
    (set-table-constraints! table (append (table-constraints table) (list ans)))
    ans))

; table-alias attribute -> column-alias
(define (construct-column-alias table column)
  (make-column-alias (column-type column) 
                     (symbol-append (table-alias-name table) '_ (column-name column))
                     table
                     column))

; symbol expression -> expression-alias
(define (construct-expression-alias name expr)
  (make-expression-alias (expression-type expr) name expr))

; sql-quotable -> literal
(define construct-literal
  (let ([type:boolean  (make-boolean-type)]
        [type:integer  (make-integer-type)]
        [type:real     (make-real-type)]
        [type:string   (make-string-type #f)]
        [type:symbol   (make-symbol-type #f)]
        [type:time-utc (make-time-utc-type)]
        [type:time-tai (make-time-tai-type)])
    (match-lambda
      [(? boolean? val)  (make-literal type:boolean  val)]
      [(? integer? val)  (make-literal type:integer  val)]
      [(? real? val)     (make-literal type:real     val)]
      [(? string? val)   (make-literal type:string   val)]
      [(? symbol? val)   (make-literal type:symbol   val)]
      [(? time-utc? val) (make-literal type:time-utc val)]
      [(? time-tai? val) (make-literal type:time-tai val)])))

; type -> literal
(define (construct-null-literal type)
  (make-literal type (type-null type)))

; Predicates -----------------------------------

; any -> boolean
(define (relation+select? item)
  (or (relation? item) (select? item)))

; any -> boolean
(define (boolean-expression?   expr) (and (expression? expr) (boolean-type?   (expression-type expr))))
(define (integer-expression?   expr) (and (expression? expr) (integer-type?   (expression-type expr))))
(define (real-expression?      expr) (and (expression? expr) (real-type?      (expression-type expr))))
(define (numeric-expression?   expr) (and (expression? expr) (numeric-type?   (expression-type expr))))
(define (string-expression?    expr) (and (expression? expr) (string-type?    (expression-type expr))))
(define (symbol-expression?    expr) (and (expression? expr) (symbol-type?    (expression-type expr))))
(define (character-expression? expr) (and (expression? expr) (character-type? (expression-type expr))))
(define (time-utc-expression?  expr) (and (expression? expr) (time-utc-type?  (expression-type expr))))
(define (time-tai-expression?  expr) (and (expression? expr) (time-tai-type?  (expression-type expr))))
(define (temporal-expression?  expr) (and (expression? expr) (temporal-type?  (expression-type expr))))

; Accessors and mutators -------------------------

; relation symbol -> (U attribute #f)
(define (relation-attribute rel name)
  (ormap (lambda (attr)
           (and (eq? (attribute-name attr) name) attr))
         (relation-attributes rel)))

; relation -> symbol
(define relation-name
  (match-lambda
    [(? table-alias?  alias) (table-alias-name  alias)]
    [(? select-alias? alias) (select-alias-name alias)]))

; relation -> (listof attribute)
(define relation-attributes
  (match-lambda
    [(? table-alias? alias)
     (map (cut construct-column-alias alias <>)
          (table-columns (table-alias-table alias)))]
    [(? select-alias? alias)
     (select-what (select-alias-select alias))]
    [(? join? join)
     (append (relation-attributes (join-left join))
             (relation-attributes (join-right join)))]))

; table symbol -> (U column #f)
(define (table-column table name)
  (ormap (lambda (col)
           (and (eq? (column-name col) name) col))
         (table-columns table)))

; table symbol -> (U constraint #f)
(define (table-constraint table name)
  (ormap (lambda (con)
           (and (eq? (constraint-name con) name) con))
         (table-constraints table)))

;  (U expression source select boolean integer real string symbol time-tai time-utc)
; ->
;  (U expression source)
(define (sql-quote arg)
  (cond [(expression?   arg) arg]
        [(relation?     arg) arg]
        [(sql-quotable? arg) (construct-literal arg)]
        [(select?       arg) (make-select-alias (gensym/interned 'subq) arg)]
        [else                (raise-type-error 'sql-quote "argument cannot be quoted" arg)]))

; any -> boolean
(define (sql-quotable? item)
  (or (boolean?  item)
      (integer?  item)
      (real?     item)
      (string?   item)
      (symbol?   item)
      (time-tai? item)
      (time-utc? item)))

; any -> boolean
(define (sql+quotable? item)
  (or (expression?   item)
      (sql-quotable? item)
      (select?       item)
      (select-alias? item)))

; Provide statements -----------------------------

(provide (except-out (all-from-out "struct-internal.ss")
                     make-table
                     ; make-column
                     make-primary-key
                     make-foreign-key
                     make-unique-constraint
                     make-check-constraint
                     make-column-alias
                     make-expression-alias
                     make-literal))

(provide/contract
 [rename construct-table make-table                        (-> symbol? table?)]
 [add-table-column!                                        (->* (table? symbol? type?) (boolean? any/c) column?)]
 [add-table-primary-key!                                   (->* (table? symbol? column?) () #:rest (listof column?) primary-key?)]
 [add-table-foreign-key!                                   (-> table? symbol? (or/c column? column-alias?) (or/c column? column-alias?) foreign-key?)]
 [add-table-unique-constraint!                             (->* (table? symbol? column?) () #:rest (listof column?) unique-constraint?)]
 [add-table-check-constraint!                              (-> table? symbol? expression? check-constraint?)]
 [rename construct-column-alias      make-column-alias     (-> table-alias? column? column-alias?)]
 [rename construct-expression-alias  make-expression-alias (-> symbol? expression? expression-alias?)]
 [rename construct-literal           make-literal          (-> sql+quotable? literal?)]
 [rename construct-null-literal      make-null-literal     (-> type? literal?)]
 [relation+select?                                         (-> (or/c relation? select?) boolean?)]
 [sql-quotable?                                            (-> any/c boolean?)]
 [sql+quotable?                                            (-> any/c boolean?)]
 [boolean-expression?                                      (-> any/c boolean?)]
 [integer-expression?                                      (-> any/c boolean?)]
 [real-expression?                                         (-> any/c boolean?)]
 [string-expression?                                       (-> any/c boolean?)]
 [symbol-expression?                                       (-> any/c boolean?)]
 [time-utc-expression?                                     (-> any/c boolean?)]
 [time-tai-expression?                                     (-> any/c boolean?)]
 [numeric-expression?                                      (-> any/c boolean?)]
 [character-expression?                                    (-> any/c boolean?)]
 [temporal-expression?                                     (-> any/c boolean?)]
 [relation-name                                            (-> relation? symbol?)]
 [relation-attribute                                       (-> relation? symbol? (or/c attribute? #f))]
 [relation-attributes                                      (-> relation? (listof attribute?))]
 [table-column                                             (-> table? symbol? (or/c column? #f))]
 [table-constraint                                         (-> table? symbol? (or/c constraint? #f))]
 [sql-quote                                                (-> (or/c expression? relation? select? sql-quotable?)
                                                               (or/c expression? relation?))])
