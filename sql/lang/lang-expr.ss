#lang scheme/base

(require "../../base.ss")

(require (unlib-in [list symbol])
         "lang-internal.ss"
         "struct.ss"
         "type.ss")

; Type shortcuts (for use below) -----------------

(define type:boolean  (make-boolean-type))
(define type:integer  (make-integer-type))
(define type:real     (make-real-type))
(define type:string   (make-string-type #f))
(define type:symbol   (make-symbol-type #f))
(define type:time-utc (make-time-utc-type))
(define type:time-tai (make-time-tai-type))

; Expressions ------------------------------------
#|
; column-alias -> aggregate
(define (sql:count arg)
  (let ([arg (sql-lift arg)])
    (make-aggregate type:integer 'count (list arg))))

; [relation] -> aggregate
(define sql:count*
  (case-lambda
    [()      (make-aggregate type:integer 'count* null)]
    [(alias) (make-aggregate type:integer 'count* (list alias))]))

; expression+quotable -> aggregate
(define (sql:min arg)     
  (let ([arg (sql-lift arg)])
    (make-aggregate (expression-type arg) 'min (list arg))))

; expression+quotable -> aggregate
(define (sql:max arg)     
  (let ([arg (sql-lift arg)])
    (make-aggregate (expression-type arg) 'max (list arg))))

; expression+quotable -> aggregate
(define sql:average
  (let ([type type:real])
    (lambda (alias)     
      (let ([arg (sql-lift alias)])
        (if (numeric-expression? arg)
            (make-aggregate type 'average (list arg))
            (raise-type-error 'average (list arg)))))))
|#

; expression+quotable ... -> function
(define-function (sql:and . args)
  [(andmap boolean-expression? args) type:boolean])

; expression+quotable ... -> function
(define-function (sql:or  . args)
  [(andmap boolean-expression? args) type:boolean])

; expression+quotable -> function
(define-function (sql:not arg)
  [(boolean-expression? arg) type:boolean])

; expression+quotable ... -> function
(define-function (sql:+ . args)
  [(andmap integer-expression? args)  type:integer]
  [(andmap numeric-expression? args)  type:real]
  [(andmap time-tai-expression? args) type:time-tai]
  [(andmap temporal-expression? args) type:time-utc])

; expression+quotable ... -> function
(define-function (sql:- . args)
  [(andmap integer-expression? args)  type:integer]
  [(andmap numeric-expression? args)  type:real]
  [(andmap time-tai-expression? args) type:time-tai]
  [(andmap temporal-expression? args) type:time-utc])

; expression+quotable ... -> function
(define-function (sql:* . args)
  [(andmap integer-expression? args) type:integer]
  [(andmap numeric-expression? args) type:real])

; expression+quotable ... -> function
(define-function (sql:/ arg1 arg2)
  [(and (numeric-expression? arg1) (numeric-expression? arg2))  type:real])

; expression+quotable -> function
(define-function (sql:abs arg)
  [(integer-expression? arg) type:integer]
  [(numeric-expression? arg) type:real])

; expression+quotable -> function
(define-function (sql:floor arg)
  [(numeric-expression? arg) type:integer])

; expression+quotable -> function
(define-function (sql:ceiling arg)
  [(numeric-expression? arg) type:integer])

; expression+quotable -> function
(define-function (sql:round arg)
  [(numeric-expression? arg) type:integer])

; TODO : Functions from http://www.postgresql.org/docs/8.2/static/functions-math.html

; expression+quotable expression+quotable -> function
(define-function (sql:like arg1 arg2)
  [(and (character-expression? arg1) (character-expression? arg2)) type:boolean])

; expression+quotable expression+quotable -> function
(define-function (sql:regexp-match arg1 arg2)
  [(and (character-expression? arg1) (character-expression? arg2)) type:boolean])

; expression+quotable expression+quotable -> function
(define-function (sql:regexp-match-ci arg1 arg2)
  [(and (character-expression? arg1) (character-expression? arg2)) type:boolean])

; expression+quotable ... -> function
(define-function (sql:string-append . args)
  [(andmap symbol-expression? args) type:symbol]
  [(andmap character-expression? args) type:string])

; expression+quotable ... -> function
(define-function (sql:string-replace arg1 arg2 arg3)
  [(andmap symbol-expression? (list arg1 arg2 arg3)) type:symbol]
  [(andmap character-expression? (list arg1 arg2 arg3)) type:string])

; expression+quotable ... -> function
(define-function (sql:regexp-replace arg1 arg2 arg3)
  [(andmap symbol-expression? (list arg1 arg2 arg3)) type:symbol]
  [(andmap character-expression? (list arg1 arg2 arg3)) type:string])

; expression+quotable ... -> function
(define-function (sql:regexp-replace-ci arg1 arg2 arg3)
  [(andmap symbol-expression? (list arg1 arg2 arg3)) type:symbol]
  [(andmap character-expression? (list arg1 arg2 arg3)) type:string])

; expression+quotable ... -> function
(define-function (sql:regexp-replace* arg1 arg2 arg3)
  [(andmap symbol-expression? (list arg1 arg2 arg3)) type:symbol]
  [(andmap character-expression? (list arg1 arg2 arg3)) type:string])

; expression+quotable ... -> function
(define-function (sql:regexp-replace*-ci arg1 arg2 arg3)
  [(andmap symbol-expression? (list arg1 arg2 arg3)) type:symbol]
  [(andmap character-expression? (list arg1 arg2 arg3)) type:string])

; expression+quotable expression+quotable -> function
(define-function (sql:= arg1 arg2)
  [#t type:boolean])

; expression+quotable expression+quotable -> function
(define-function (sql:<> arg1 arg2)
  [#t type:boolean])

; expression+quotable expression+quotable -> function
(define-function (sql:< arg1 arg2)
  [(and (boolean-expression? arg1) (boolean-expression? arg2))     type:boolean]
  [(and (numeric-expression? arg1) (numeric-expression? arg2))     type:boolean]
  [(and (character-expression? arg1) (character-expression? arg2)) type:boolean]
  [(and (temporal-expression? arg1) (temporal-expression? arg2))   type:boolean])

; expression+quotable expression+quotable -> function
(define-function (sql:> arg1 arg2)
  [(and (boolean-expression? arg1) (boolean-expression? arg2))     type:boolean]
  [(and (numeric-expression? arg1) (numeric-expression? arg2))     type:boolean]
  [(and (character-expression? arg1) (character-expression? arg2)) type:boolean]
  [(and (temporal-expression? arg1) (temporal-expression? arg2))   type:boolean])

; expression+quotable expression+quotable -> function
(define-function (sql:>= arg1 arg2)
  [(and (boolean-expression? arg1) (boolean-expression? arg2))     type:boolean]
  [(and (numeric-expression? arg1) (numeric-expression? arg2))     type:boolean]
  [(and (character-expression? arg1) (character-expression? arg2)) type:boolean]
  [(and (temporal-expression? arg1) (temporal-expression? arg2))   type:boolean])

; expression+quotable expression+quotable -> function
(define-function (sql:<= arg1 arg2)
  [(and (boolean-expression? arg1) (boolean-expression? arg2))     type:boolean]
  [(and (numeric-expression? arg1) (numeric-expression? arg2))     type:boolean]
  [(and (character-expression? arg1) (character-expression? arg2)) type:boolean]
  [(and (temporal-expression? arg1) (temporal-expression? arg2))   type:boolean])

; expression+quotable -> function
(define-function (sql:null? arg)
  [#t type:boolean])

; expression+quotable ... -> function
(define-function (sql:coalesce . args)
  [(andmap boolean-expression? args)  type:boolean]
  [(andmap integer-expression? args)  type:integer]
  [(andmap numeric-expression? args)  type:real]
  [(andmap symbol-expression? args)   type:symbol]
  [(andmap string-expression? args)   type:string]
  [(andmap time-tai-expression? args) type:time-tai]
  [(andmap temporal-expression? args) type:time-utc])

; expression+quotable expression+quotable -> function
(define-function (sql:->string arg1 arg2)
  [(string-expression? arg2) type:string])

; expression+quotable expression+quotable -> function
(define-function (sql:->symbol arg1 arg2)
  [(string-expression? arg2) type:symbol])

; expression+quotable (U select-alias (listof expression+quotable)) -> function
(define (sql:in arg1 arg2)
  (let* ([arg1  (sql-lift arg1)]
         [arg2  (if (list? arg2)
                    (map sql-lift arg2)
                    arg2)]
         [type1 (expression-type arg1)]
         [type2 (cond [(null? arg2)  type1] ; hack to make sure the type check passes
                      [(pair? arg2)  (let ([type2 (expression-type (car arg2))])
                                       (unless (andmap (cut type-compatible? type2 <>)
                                                       (map expression-type arg2))
                                         (raise-exn exn:fail:contract
                                           (format "sql:in: list elements must all be of the same type: ~a" arg2)))
                                       type2)]
                      [(select? arg2) (let ([columns (select-what arg2)])
                                        (unless (= (length columns) 1)
                                          (raise-exn exn:fail:contract
                                            (format "sql:in: subselect must have exactly one column: ~a" arg2)))
                                        (expression-type (car columns)))])])
    (unless (type-compatible? type1 type2)
      (raise-exn exn:fail:contract
        (format "sql:in: type mismatch: argument types do not match: ~a ~a" type1 type2)))
    (make-function type:boolean 'in (list arg1 arg2))))

; expression+quotable expression+quotable [expression+quotable] -> function
(define sql:if
  (case-lambda
    [(test pos)
     (define pos* (sql-lift pos))
     (sql:if test pos* (sql:null (expression-type pos*)))]
    [(test pos neg)
     (let* ([test (sql-lift test)]
            [pos  (sql-lift pos)]
            [neg  (sql-lift neg)]
            [type (cond [(and (boolean-expression? test) (andmap boolean-expression?   (list pos neg))) type:boolean]
                        [(and (boolean-expression? test) (andmap integer-expression?   (list pos neg))) type:integer]
                        [(and (boolean-expression? test) (andmap numeric-expression?   (list pos neg))) type:real]
                        [(and (boolean-expression? test) (andmap symbol-expression?    (list pos neg))) type:symbol]
                        [(and (boolean-expression? test) (andmap character-expression? (list pos neg))) type:string]
                        [(and (boolean-expression? test) (andmap time-tai-expression?  (list pos neg))) type:time-tai]
                        [(and (boolean-expression? test) (andmap temporal-expression?  (list pos neg))) type:time-utc]
                        [else (error (format "~a not defined for the types: ~s"
                                             'id
                                             (map expression-type (list test pos neg))))])])
       (make-function type 'if (list test pos neg)))]))

; (_ [expt expr] ... [else expr])
(define-syntax sql:cond
  (syntax-rules (else)
    [(_ [test expr1])
     (sql:if test expr1)]
    [(_ [test expr1] [else expr2])
     (sql:if test expr1 expr2)]
    [(_ [test1 expr1] [test2 expr2] ...)
     (sql:if test1 expr1 (sql:cond [test2 expr2] ...))]))

; quotable -> expression
(define sql:literal make-literal)

; type -> literal
(define (sql:null type)
  (make-null-literal type))

; Order ----------------------------------------

; expression+quotable (U 'asc 'desc) -> order
(define (sql:order expr dir)
  (make-order (sql-lift expr) dir))

; expression -> order
(define sql:asc  (cut sql:order <> 'asc))
(define sql:desc (cut sql:order <> 'desc))

; Provide statements -----------------------------

(provide (rename-out [sql:alias alias]
                     [sql:cond  cond]))

(provide/contract
 [rename sql:select          select          (->* (#:from     (or/c relation? select?))
                                                  (#:what     (or/c expression? relation?
                                                                    (listof (or/c expression? relation?))
                                                                    false/c)
                                                              #:distinct (or/c expression? (listof expression?) boolean?)
                                                              #:where    (or/c expression? false/c)
                                                              #:group    (listof (or/c column? relation?))
                                                              #:order    (listof order?)
                                                              #:having   (or/c expression? false/c)
                                                              #:limit    (or/c integer? false/c)
                                                              #:offset   (or/c integer? false/c))
                                                  select?)]
 [rename sql:select/internal select/internal (-> (or/c expression? relation? (listof (or/c expression? relation?)) false/c)
                                                 (or/c expression? (listof expression?) boolean?)
                                                 (or/c relation? select?)
                                                 (or/c expression? false/c)
                                                 (listof (or/c column? relation?))
                                                 (listof order?)
                                                 (or/c expression? false/c)
                                                 (or/c integer? false/c)
                                                 (or/c integer? false/c)
                                                 select?)]
 [rename sql:count           count           (-> column-alias? aggregate?)]
 [rename sql:count*          count*          (->* () ((or/c table-alias? select-alias?)) aggregate?)]
 [rename sql:min             min             (-> quotable? aggregate?)]
 [rename sql:max             max             (-> quotable? aggregate?)]
 [rename sql:average         average         (-> quotable? aggregate?)]
 [rename sql:inner           inner           (-> relation+select? relation+select? quotable? join?)]
 [rename sql:left            left            (-> relation+select? relation+select? quotable? join?)]
 [rename sql:right           right           (-> relation+select? relation+select? quotable? join?)]
 [rename sql:outer           outer           (-> relation+select? relation+select? join?)]
 [rename sql:in              in              (-> quotable? (or/c select? (listof quotable?)) function?)]
 [rename sql:if              if              (->* (quotable? quotable?) (quotable?) function?)]
 [rename sql:literal         literal         (-> quotable? literal?)]
 [rename sql:null            null            (-> type? literal?)]
 [rename sql:order           order           (-> quotable? (symbols 'asc 'desc) order?)]
 [rename sql:asc             asc             (-> quotable? order?)]
 [rename sql:desc            desc            (-> quotable? order?)])
