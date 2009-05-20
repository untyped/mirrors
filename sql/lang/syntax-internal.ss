#lang scheme/base

(require "../../base.ss")

(require srfi/13
         srfi/26
         (unlib-in syntax)
         "expression.ss"
         "identifier.ss"
         "struct.ss"
         (for-template scheme/base
                       "expression.ss"))

; Helpers ----------------------------------------

(define (or-expand message [backtrace null])
  (match-lambda*
    [(list)
     (lambda (stx)
       (raise-syntax-error
        'mirrors-sql
        (format "no match for rule: ~a" backtrace)) stx)]
    [(list-rest expand rest)
     (lambda (stx)
       (with-handlers
           ([exn? (lambda (exn)
                    (define backtrace*
                      (cons (exn-message exn) backtrace))
                    (define next-expand
                      (or-expand message backtrace*))
                    ((apply next-expand rest) stx))])
         (expand stx)))]))

; Expansions -------------------------------------

(define (expand-unquote stx)
  (syntax-case* stx (unquote) symbolic-identifier=?
    [(unquote expr) #'expr]))

(define (expand-identifier stx)
  (syntax-case stx ()
    [id (identifier? #'id)
        (if (eq? (sql-identifier-type #'id) 'alias)
            #'id
            (raise-syntax-error
             'expand-identifier
             (format "not an SQL alias: ~a" (syntax->datum #'id))
             stx
             #'id))]))

(define (expand-literal stx)
  (syntax-case* stx (quote quasiquote) symbolic-identifier=?
    [(quote literal)      (sql-quotable? (syntax->datum #'literal))
                          #'(sql-literal (quote literal))]
    [(quasiquote literal) (sql-quotable? (syntax->datum #'literal))
                          #'(sql-literal (quasiquote literal))]
    [literal              (and (not (identifier? #'literal)) (sql-quotable? (syntax->datum #'literal)))
                          #'(sql-literal literal)]))

(define (expand-true+false stx)
  (syntax-case* stx () symbolic-identifier=?
    [#t #'#t]
    [#f #'#f]))

(define expand-literal+unquote
  ((or-expand "literal+unquote")
   expand-literal
   expand-unquote))

(define (expand-integer stx)
  (syntax-case* stx (quote quasiquote) symbolic-identifier=?
    [(quote literal)      (integer? (syntax->datum #'literal)) #'literal]
    [(quasiquote literal) (integer? (syntax->datum #'literal)) #'literal]
    [literal              (integer? (syntax->datum #'literal)) #'literal]))

(define expand-integer+unquote
  ((or-expand "integer+unquote")
   expand-integer
   expand-unquote))

(define (expand-source stx)
  (printf "source~n")
  (syntax-case* stx (outer inner left right select) symbolic-identifier=?
    [(outer a b)      #`(sql-outer #,(expand-source+unquote #'a) #,(expand-source+unquote #'b))]
    [(inner a b on)   #`(sql-inner #,(expand-source+unquote #'a) #,(expand-source+unquote #'b) #,(expand-expression+unquote #'on))]
    [(left  a b on)   #`(sql-left  #,(expand-source+unquote #'a) #,(expand-source+unquote #'b) #,(expand-expression+unquote #'on))]
    [(right a b on)   #`(sql-right #,(expand-source+unquote #'a) #,(expand-source+unquote #'b) #,(expand-expression+unquote #'on))]
    [(select arg ...) (expand-select #'(select arg ...))]
    [id               (identifier? #'id)
                      (expand-identifier #'id)]))

(define expand-source+unquote
  ((or-expand "source+unquote")
   expand-source
   expand-unquote))

(define (expand-function stx)
  (syntax-case stx ()
    [(id arg ...)
     (with-syntax ([sql-id (make-id #'id 'sql- #'id)])
       (printf "sql-function ~s ~s~n" #'sql-id (sql-identifier-type #'sql-id))
       (printf "sql-+ ~s ~s~n" #'sql-+ (sql-identifier-type #'sql-+))
       (if (eq? (sql-identifier-type #'sql-id) 'function)
           (quasisyntax/loc stx
             (sql-id #,@(map expand-expression (syntax->list #'(arg ...)))))
           (raise-syntax-error #f "not an sql function" stx #'id)))]))

(define expand-expression
  ((or-expand "expression")
   expand-function
   expand-identifier
   expand-literal))

(define expand-expression+unquote
  ((or-expand "expression+unquote")
   expand-expression
   expand-unquote))

(define expand-distinct
  ((or-expand "expression")
   expand-true+false
   expand-function
   expand-identifier
   expand-literal))

(define expand-distinct+unquote
  ((or-expand "distinct+unquote")
   expand-distinct
   expand-unquote))

(define expand-column
  ((or-expand "column")
   expand-expression
   expand-identifier))

(define expand-column+unquote
  ((or-expand "column+unquote")
   expand-column
   expand-unquote))

(define (expand-column-list stx)
  #`(list #,@(map expand-column+unquote (syntax->list stx))))

(define expand-column-list+unquote
  ((or-expand "column-list+unquote")
   expand-column-list
   expand-unquote))

(define expand-column+column-list
  ((or-expand "column+column-list")
   expand-column+unquote
   expand-column-list))

(define expand-column+column-list+unquote
  ((or-expand "column+column-list")
   expand-column+column-list
   expand-unquote))

(define (expand-direction stx)
  (syntax-case* stx (quote quasiquote asc desc) symbolic-identifier=?
    [(quote asc)       #'(quote asc)]
    [(quote desc)      #'(quote desc)]
    [(quasiquote asc)  #'(quasiquote asc)]
    [(quasiquote desc) #'(quasiquote desc)]))

(define expand-direction+unquote
  ((or-expand "direction+unquote")
   expand-direction
   expand-unquote))

(define (expand-order stx)
  (syntax-case* stx (asc desc ord order) symbolic-identifier=?
    [(asc   expr)     #`(sql-asc   #,(expand-expression+unquote #'expr))]
    [(desc  expr)     #`(sql-desc  #,(expand-expression+unquote #'expr))]
    [(ord   expr dir) #`(sql-order #,(expand-expression+unquote #'expr) #,(expand-direction+unquote #'dir))]
    [(order expr dir) #`(sql-order #,(expand-expression+unquote #'expr) #,(expand-direction+unquote #'dir))]))

(define expand-order+unquote
  ((or-expand "order+unquote")
   expand-order
   expand-unquote))

(define (expand-order-list stx)
  #`(list #,@(map expand-order+unquote (syntax->list stx))))

(define expand-order-list+unquote
  ((or-expand "order-list+unquote")
   expand-order-list
   expand-unquote))

(define (expand-select-arguments select-stx args)
  (printf "select~n")
  (match args
    ; No arguments left:
    [(list) null]
    ; Only a single argument left (must be an error):
    [(list (and key-stx (app syntax->datum key)))
     (if (keyword? key)
         (raise-syntax-error #f (format "Missing value for ~s" key) select-stx key-stx)
         (raise-syntax-error #f (format "Expected keyword, received ~s" key) select-stx key-stx))]
    ; Keyword and value arguments present:
    [(list-rest (and key-stx (app syntax->datum key))
                (and arg-stx (app syntax->datum arg))
                rest)
     (cond [(not (keyword? key))  (raise-syntax-error #f (format "Expected keyword, received ~s" key) select-stx key-stx)]
           [(keyword? arg)        (raise-syntax-error #f (format "Missing value for ~s" arg) select-stx arg-stx)]
           ; TODO : Check arg-stx is a list
           [(eq? key '#:what)     (list* key-stx 
                                         (expand-column+column-list+unquote arg-stx)
                                         (expand-select-arguments select-stx rest))]
           [(eq? key '#:from)     (list* key-stx
                                         (expand-source+unquote arg-stx)
                                         (expand-select-arguments select-stx rest))]
           [(eq? key '#:where)    (list* key-stx
                                         (expand-expression+unquote arg-stx)
                                         (expand-select-arguments select-stx rest))]
           ; TODO : Check arg-stx is a list
           [(eq? key '#:group)    (list* key-stx
                                         (expand-column-list+unquote arg-stx)
                                         (expand-select-arguments select-stx rest))]
           ; TODO : Check arg-stx is a list
           [(eq? key '#:order)    (list* key-stx
                                         (expand-order-list+unquote arg-stx)
                                         (expand-select-arguments select-stx rest))]
           [(eq? key '#:having)   (list* key-stx
                                         (expand-expression+unquote arg-stx)
                                         (expand-select-arguments select-stx rest))]
           ; TODO : Only allow integers
           [(eq? key '#:limit)    (list* key-stx
                                         (expand-integer+unquote arg-stx)
                                         (expand-select-arguments select-stx rest))]
           [(eq? key '#:offset)   (list* key-stx
                                         (expand-integer+unquote arg-stx)
                                         (expand-select-arguments select-stx rest))]
           [(eq? key '#:distinct) (list* key-stx
                                         (expand-distinct+unquote arg-stx)
                                         (expand-select-arguments select-stx rest))]
           [else                  (raise-syntax-error #f (format "Unknown select keyword: ~s" key) select-stx key-stx)])]))

(define (expand-select stx)
  (syntax-case* stx (select) symbolic-identifier=?
    [(select kw+expr ...)
     #`(sql-select #,@(expand-select-arguments stx (syntax->list #'(kw+expr ...))))]))

(define expand-top-level
  ((or-expand "sql")
   expand-select
   expand-column
   expand-source
   expand-expression
   expand-order))

; Provide statements -----------------------------

(provide expand-top-level)
