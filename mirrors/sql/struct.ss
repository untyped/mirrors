#lang scheme/base

(require scheme/contract
         scheme/match
         srfi/19/time
         srfi/26/cut
         (planet untyped/unlib:3/time)
         (planet untyped/unlib:3/symbol)
         "../base.ss"
         "../era/era.ss")

; ***** NOTE *****
; The terms "table" and "attribute" are used here
; to refer to parts of the select representation. The
; "table" and "attribute" from era/era.ss are referred
; to as "era:table" and "era:attribute".
; ****************

; Sources --------------------------------------

(define-struct source () #:transparent)

(define-struct (source-alias source) (name value) #:transparent)

; (struct symbol table)
(define-struct (table-alias source-alias) () #:transparent)

; (struct symbol select)
(define-struct (select-alias source-alias) () #:transparent)

; source-alias -> (listof column)
(define (source-alias-columns alias)
  (if (table-alias? alias)
      (map (cut create-attribute-alias alias <>)
           (table-attributes (source-alias-value alias)))
      (select-what (source-alias-value alias))))

; (struct symbol source source (U expression #f))
(define-struct (join source) (op left right on) #:transparent)

; Expressions ------------------------------------

; (struct type)
(define-struct expression (type) #:transparent)

; (struct type symbol boolean any)
(define-struct (column expression) (name allow-null? default) #:transparent)

; (struct type table-alias attribute)
(define-struct (attribute-alias column) (table attribute) #:transparent)

; table-alias attribute -> value
(define (create-attribute-alias table attr)
  (make-attribute-alias (attribute-type attr) 
                        (symbol-append (source-alias-name table) '- (attribute-name attr))
                        table
                        attr))

; (struct type symbol expression)
(define-struct (expression-alias column) (value) #:transparent) 

; symbol expression -> value
(define (create-expression-alias name value)
  (make-expression-alias (expression-type value) name value))

; (struct type symbol (listof expression)
(define-struct (function expression) (op args) #:transparent)

; (struct type symbol (listof (U table-alias attribute-alias)))
(define-struct (aggregate function) () #:transparent)

; (struct type any)
(define-struct (literal expression) (value) #:transparent)

; literal-value -> literal
(define (create-literal val)
  (cond [(boolean? val)  (make-literal type:boolean  val)]
        [(integer? val)  (make-literal type:integer  val)]
        [(real? val)     (make-literal type:real     val)]
        [(string? val)   (make-literal type:string   val)]
        [(symbol? val)   (make-literal type:symbol   val)]
        [(time-utc? val) (make-literal type:time-utc val)]
        [(time-tai? val) (make-literal type:time-tai val)]
        [else            (raise-exn exn:fail:contract
                           (format "Expected (U boolean integer real string symbol time-tai time-utc), received ~s" val))]))

; type -> literal
(define (create-null type)
  (make-literal type (type-null type)))

; Ordering ---------------------------------------

; ORDER clauses are lists of order structures in descending order of importance.

; (struct expression (U 'asc 'desc))
(define-struct order (expression direction) #:transparent)

; Queries ----------------------------------------

; (struct (listof column)
;         (U expression #t #f)
;         source
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

; Predicates -----------------------------------

; any -> boolean
(define (source+select? item)
  (or (source? item)
      (select? item)))

; any -> boolean
(define (literal-value? item)
  (or (boolean? item)
      (integer? item)
      (real? item)
      (string? item)
      (symbol? item)
      (time-tai? item)
      (time-utc? item)))

; any -> boolean
(define (quotable? item)
  (or (expression? item)
      (literal-value? item)
      (select? item)
      (select-alias? item)))

; (U expression source select boolean integer real string symbol time-tai time-utc) -> (U expression source)
(define (quote-argument arg)
  (cond [(expression? arg)            arg]
        [(source? arg)                arg]
        [(literal-value? arg)         (create-literal arg)]
        [(select? arg)                 (make-select-alias (string->symbol (symbol->string (gensym 'subq))) arg)]
        [else (raise-exn exn:fail:contract
                (format "Expected (opt-listof (U expression select boolean integer real string symbol time-tai time-utc)), received ~s" arg))]))

; Provide statements --------------------------

; contract
;
; "in" takes some weird argument types:
(define function-arg/c
  (or/c expression? select-alias? table-alias? select? (listof expression?)))

; contract
(define aggregate-arg/c
  (or/c function-arg/c table-alias? select-alias?))

; contract
(define source/c
  (or/c table-alias? select-alias?))

; Provide statements -----------------------------

(provide (except-out (struct-out attribute-alias) make-attribute-alias)
         (except-out (struct-out expression-alias) make-expression-alias)
         (except-out (struct-out literal) make-literal)
         (rename-out (create-attribute-alias make-attribute-alias))
         (rename-out (create-expression-alias make-expression-alias))
         (rename-out (create-literal make-literal))
         (rename-out (create-null make-null))
         quotable?
         quote-argument
         source+select?
         source/c)

(provide/contract
 [struct source                        ()]
 [struct (source-alias source)         ([name symbol?] [value (or/c table? select?)])]
 [struct (table-alias source-alias)   ([name symbol?] [value table?])]
 [struct (select-alias source-alias)    ([name symbol?] [value select?])]
 [struct (join source)                 ([op symbol?] [left source?] [right source?] [on (or/c expression? false/c)])]
 [struct expression                    ([type type?])]
 [struct (column expression)           ([type type?] [name symbol?])]
 [struct (function expression)         ([type type?] [op symbol?] [args (listof function-arg/c)])]
 [struct (aggregate function)          ([type type?] [op symbol?] [args (listof function-arg/c)])]
 [struct order                         ([expression expression?] [direction (symbols 'asc 'desc)])]
 [struct select                         ([what             (listof column?)]
                                        [distinct         (or/c (listof expression?) false/c)]
                                        [from             source?]
                                        [where            (or/c expression? false/c)]
                                        [group            (listof expression?)]
                                        [order            (listof order?)]
                                        [having           (or/c expression? false/c)]
                                        [limit            (or/c integer? false/c)]
                                        [offset           (or/c integer? false/c)]
                                        [local-columns    (listof column?)]
                                        [imported-columns (listof column?)]
                                        [extract-info     (or/c table? type? (listof (or/c table? type?)))])]
 [source-alias-columns                 (-> source-alias? (listof column?))])
