#lang scheme/base

(require "../../base.ss"
         "type.ss")

; General nomenclature:
;   - databases contain TABLES, and these contain COLUMNS
;   - TABLES are a type of RELATION
;   - COLUMNS are a type of ATTRIBUTE
;   - data is queried by SELECTing it from a SOURCE
;   - SOURCES are ALIASES of tables, ALIASES of other SELECTS 

; Tables -----------------------------------------

; (struct symbol (listof column) (listof constraint))
(define-struct table (name [columns #:mutable] [constraints #:mutable]) #:transparent)

; (struct table symbol type boolean any)
(define-struct column (table name type not-null? default) #:transparent)

; (struct table symbol)
(define-struct constraint (table name) #:transparent)

; (struct table symbol (listof column))
(define-struct (primary-key constraint) (columns) #:transparent)

; (struct table symbol column)
(define-struct (foreign-key constraint) (local foreign) #:transparent)

; (struct table symbol (listof column))
(define-struct (unique-constraint constraint) (columns) #:transparent)

; (struct table symbol expression)
(define-struct (check-constraint constraint) (expression) #:transparent)

; Relations --------------------------------------

; Sources appear in FROM clauses.
;
; There are three types of relation:
;   - table aliases;
;   - select statement aliases;
;   - joins over other relations.

; (struct)
(define-struct relation () #:transparent)

; (struct symbol table)
(define-struct (table-alias relation) (name table) #:transparent)

; (struct symbol select)
(define-struct (select-alias relation) (name select) #:transparent)

; (struct symbol relation relation (U expression #f))
(define-struct (join relation) (op left right on) #:transparent)

; Expressions ------------------------------------

; Expressions appear in WHERE and ON clauses, CHECK constraints and many other places.
; 
; There are several types of expression:
;   - functions (+, -, string-length and so on);
;   - aggregate functions (min, max, count and so on);
;   - references to column and expression aliases;
;   - literal values.

; (struct type)
(define-struct expression (type) #:transparent)

; (struct type symbol)
(define-struct (attribute expression) (name) #:transparent)

; (struct type table-alias attribute)
(define-struct (column-alias attribute) (table-alias column) #:transparent)

; (struct type symbol expression)
(define-struct (expression-alias attribute) (value) #:transparent) 

; (struct type symbol (listof expression)
(define-struct (function expression) (op args) #:transparent)

; (struct type symbol (listof (U table-alias attribute-alias)))
(define-struct (aggregate function) () #:transparent)

; (struct type any)
(define-struct (literal expression) (value) #:transparent)

; Ordering ---------------------------------------

; ORDER clauses are lists of order structures in descending order of importance.

; (struct expression (U 'asc 'desc))
(define-struct order (expression direction) #:transparent)

; Queries ----------------------------------------

; (struct)
(define-struct query () #:transparent)

; (struct table)
(define-struct (create query) (table) #:transparent)

; (struct table)
(define-struct (drop query) (table) #:transparent)

; (struct table-alias (listof column-alias) (listof (listof any)))
(define-struct (insert query) (table columns data) #:transparent)

; (struct table-alias (U relation #f) (U expression #f))
(define-struct (delete query) (table using where) #:transparent)

; (struct (listof column)
;         (U expression #t #f)
;         relation
;         (U expression #f)
;         (listof expression)
;         (listof order)
;         (U expression #f)
;         (U integer #f)
;         (U integer #f)
;         (listof column)
;         (listof column))
(define-struct select
  (what distinct from where group order having limit offset local-columns imported-columns)
  #:transparent)

; Provide statements --------------------------

; contract
;
; "in" takes some weird argument types:
(define function-arg/c
  (or/c expression? select-alias? table-alias? select? (listof expression?)))

; contract
;
; Some aggregates take simple relations as sources.
(define aggregate-arg/c
  (or/c function-arg/c table-alias? select-alias?))

(provide/contract
 [function-arg/c                       contract?]
 [aggregate-arg/c                      contract?]
 [struct table                         ([name             symbol?]
                                        [columns          (listof column?)]
                                        [constraints      (listof constraint?)])]
 [struct column                        ([table            table?]
                                        [name             symbol?]
                                        [type             type?]
                                        [not-null?        boolean?]
                                        [default          any/c])]
 [struct constraint                    ([table            table?]
                                        [name             symbol?])]
 [struct primary-key                   ([table            table?]
                                        [name             symbol?]
                                        [columns          (listof column?)])]
 [struct foreign-key                   ([table            table?]
                                        [name             symbol?]
                                        [local            column?]
                                        [foreign          column?])]
 [struct unique-constraint             ([table            table?]
                                        [name             symbol?]
                                        [columns          (listof column?)])]
 [struct check-constraint              ([table            table?]
                                        [name             symbol?]
                                        [expression       expression?])]
 [struct relation                      ()]
 [struct (table-alias relation)        ([name             symbol?]
                                        [table            table?])]
 [struct (select-alias relation)       ([name             symbol?]
                                        [select           select?])]
 [struct (join relation)               ([op               symbol?]
                                        [left             relation?]
                                        [right            relation?]
                                        [on               (or/c expression? false/c)])]
 [struct expression                    ([type             type?])]
 [struct (attribute expression)        ([type             type?]
                                        [name             symbol?])]
 [struct (column-alias attribute)      ([type             type?]
                                        [name             symbol?]
                                        [table-alias      table-alias?]
                                        [column           column?])]
 [struct (expression-alias attribute)  ([type             type?]
                                        [name             symbol?]
                                        [value            expression?])]
 [struct (function expression)         ([type             type?]
                                        [op               symbol?]
                                        [args             (listof function-arg/c)])]
 [struct (aggregate function)          ([type             type?]
                                        [op               symbol?]
                                        [args             (listof function-arg/c)])]
 [struct (literal expression)          ([type             type?]
                                        [value            any/c])]
 [struct order                         ([expression       expression?]
                                        [direction        (symbols 'asc 'desc)])]
 [struct query                         ()]
 [struct (create query)                ([table            table?])]
 [struct (drop query)                  ([table            table?])]
 [struct (insert query)                ([table            table?]
                                        [columns          (listof column?)]
                                        [data             (listof list?)])]
 [struct (delete query)                ([table            table?]
                                        [using            (or/c relation? false/c)]
                                        [where            (or/c expression? false/c)])]
 [struct select                        ([what             (listof column?)]
                                        [distinct         (or/c (listof expression?) false/c)]
                                        [from             relation?]
                                        [where            (or/c expression? false/c)]
                                        [group            (listof expression?)]
                                        [order            (listof order?)]
                                        [having           (or/c expression? false/c)]
                                        [limit            (or/c integer? false/c)]
                                        [offset           (or/c integer? false/c)]
                                        [local-columns    (listof attribute?)]
                                        [imported-columns (listof attribute?)])])
