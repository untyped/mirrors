#lang scheme/base

(require "../../base.ss")

(require (for-syntax scheme/base
                     scheme/pretty)
         "define-function.ss"
         "struct.ss"
         "type.ss")

(define type:boolean  (make-boolean-type))
(define type:integer  (make-integer-type))
(define type:real     (make-real-type))
(define type:string   (make-string-type #f))
(define type:symbol   (make-symbol-type #f))
(define type:time-utc (make-time-utc-type))
(define type:time-tai (make-time-tai-type))

; column-alias -> aggregate
(define-function (count arg)
  (let ([arg (sql-quote arg)])
    (make-aggregate type:integer 'count (list arg))))

; [relation] -> aggregate
(define-function (count* [alias undefined])
  #:contract (->* () (column?) aggregate?)
  (if (undefined? alias)
      (make-aggregate type:integer 'count* (list alias))
      (make-aggregate type:integer 'count* null)))

; expression+quotable -> aggregate
(define-function (min arg)     
  #:contract (-> sql+quotable? aggregate?)
  (let ([arg (sql-quote arg)])
    (make-aggregate (expression-type arg) 'min (list arg))))

; expression+quotable -> aggregate
(define-function (max arg)
  #:contract (-> sql+quotable? aggregate?)
  (let ([arg (sql-quote arg)])
    (make-aggregate (expression-type arg) 'max (list arg))))

; expression+quotable -> aggregate
(define-function (average arg)
  #:contract (-> sql+quotable? aggregate?)
  (let ([arg (sql-quote arg)])
    (make-aggregate (type-rules [(numeric-expression? arg) type:real]) 'average arg)))

; sql+quotable ... -> function
(define-function/types (and . args)
  [(andmap boolean-expression? args) type:boolean])

; sql+quotable ... -> function
(define-function/types (or . args)
  [(andmap boolean-expression? args) type:boolean])

; sql+quotable -> function
(define-function/types (not arg)
  [(boolean-expression? arg) type:boolean])

; sql+quotable ... -> function
(define-function/types (+ . args)
  [(andmap integer-expression?  args) type:integer]
  [(andmap numeric-expression?  args) type:real]
  [(andmap time-tai-expression? args) type:time-tai]
  [(andmap temporal-expression? args) type:time-utc])

; sql+quotable ... -> function
(define-function/types (- arg1 . args)
  [(andmap integer-expression?  (list* arg1 args)) type:integer]
  [(andmap numeric-expression?  (list* arg1 args)) type:real]
  [(andmap time-tai-expression? (list* arg1 args)) type:time-tai]
  [(andmap temporal-expression? (list* arg1 args)) type:time-utc])

; sql+quotable ... -> function
(define-function/types (* arg1 . args)
  [(andmap integer-expression? (list* arg1 args)) type:integer]
  [(andmap numeric-expression? (list* arg1 args)) type:real])

; sql+quotable ... -> function
(define-function/types (/ arg1 arg2)
  [(andmap numeric-expression? (list arg1 arg2)) type:real])

; sql+quotable -> function
(define-function/types (abs arg)
  [(integer-expression? arg) type:integer]
  [(numeric-expression? arg) type:real])

; sql+quotable -> function
(define-function/types (floor arg)
  [(numeric-expression? arg) type:integer])

; sql+quotable -> function
(define-function/types (ceiling arg)
  [(numeric-expression? arg) type:integer])

; sql+quotable -> function
(define-function/types (round arg)
  [(numeric-expression? arg) type:integer])

; TODO : Functions from http://www.postgresql.org/docs/8.2/static/functions-math.html

; sql+quotable sql+quotable -> function
(define-function/types (like arg1 arg2)
  [(andmap character-expression? (list arg1 arg2)) type:boolean])

; sql+quotable sql+quotable -> function
(define-function/types (regexp-match arg1 arg2)
  [(andmap character-expression? (list arg1 arg2)) type:boolean])

; sql+quotable sql+quotable -> function
(define-function/types (regexp-match-ci arg1 arg2)
  [(andmap character-expression? (list arg1 arg2)) type:boolean])

; sql+quotable ... -> function
(define-function/types (string-append arg1 . args)
  [(andmap symbol-expression?    (list* arg1 args)) type:symbol]
  [(andmap character-expression? (list* arg1 args)) type:string])

; sql+quotable ... -> function
(define-function/types (string-replace arg1 arg2 arg3)
  [(andmap symbol-expression?    (list arg1 arg2 arg3)) type:symbol]
  [(andmap character-expression? (list arg1 arg2 arg3)) type:string])

; sql+quotable ... -> function
(define-function/types (regexp-replace arg1 arg2 arg3)
  [(andmap symbol-expression?    (list arg1 arg2 arg3)) type:symbol]
  [(andmap character-expression? (list arg1 arg2 arg3)) type:string])

; sql+quotable ... -> function
(define-function/types (regexp-replace-ci arg1 arg2 arg3)
  [(andmap symbol-expression?    (list arg1 arg2 arg3)) type:symbol]
  [(andmap character-expression? (list arg1 arg2 arg3)) type:string])

; sql+quotable ... -> function
(define-function/types (regexp-replace* arg1 arg2 arg3)
  [(andmap symbol-expression?    (list arg1 arg2 arg3)) type:symbol]
  [(andmap character-expression? (list arg1 arg2 arg3)) type:string])

; sql+quotable ... -> function
(define-function/types (regexp-replace*-ci arg1 arg2 arg3)
  [(andmap symbol-expression?    (list arg1 arg2 arg3)) type:symbol]
  [(andmap character-expression? (list arg1 arg2 arg3)) type:string])

; sql+quotable sql+quotable -> function
(define-function/types (= arg1 arg2)
  [#t type:boolean])

; sql+quotable sql+quotable -> function
(define-function/types (<> arg1 arg2)
  [#t type:boolean])

; sql+quotable sql+quotable -> function
(define-function/types (< arg1 arg2)
  [(andmap boolean-expression?   (list arg1 arg2)) type:boolean]
  [(andmap numeric-expression?   (list arg1 arg2)) type:boolean]
  [(andmap character-expression? (list arg1 arg2)) type:boolean]
  [(andmap temporal-expression?  (list arg1 arg2)) type:boolean])

; sql+quotable sql+quotable -> function
(define-function/types (> arg1 arg2)
  [(andmap boolean-expression?   (list arg1 arg2)) type:boolean]
  [(andmap numeric-expression?   (list arg1 arg2)) type:boolean]
  [(andmap character-expression? (list arg1 arg2)) type:boolean]
  [(andmap temporal-expression?  (list arg1 arg2)) type:boolean])

; sql+quotable sql+quotable -> function
(define-function/types (>= arg1 arg2)
  [(andmap boolean-expression?   (list arg1 arg2)) type:boolean]
  [(andmap numeric-expression?   (list arg1 arg2)) type:boolean]
  [(andmap character-expression? (list arg1 arg2)) type:boolean]
  [(andmap temporal-expression?  (list arg1 arg2)) type:boolean])

; sql+quotable sql+quotable -> function
(define-function/types (<= arg1 arg2)
  [(andmap boolean-expression?   (list arg1 arg2)) type:boolean]
  [(andmap numeric-expression?   (list arg1 arg2)) type:boolean]
  [(andmap character-expression? (list arg1 arg2)) type:boolean]
  [(andmap temporal-expression?  (list arg1 arg2)) type:boolean])

; sql+quotable -> function
(define-function/types (null? arg)
  [#t type:boolean])

; sql+quotable ... -> function
(define-function/types (coalesce . args)
  [(andmap boolean-expression?  args) type:boolean]
  [(andmap integer-expression?  args) type:integer]
  [(andmap numeric-expression?  args) type:real]
  [(andmap symbol-expression?   args) type:symbol]
  [(andmap string-expression?   args) type:string]
  [(andmap time-tai-expression? args) type:time-tai]
  [(andmap temporal-expression? args) type:time-utc])

; sql+quotable sql+quotable -> function
(define-function/types (->string arg1 arg2)
  [(string-expression? arg2) type:string])

; sql+quotable sql+quotable -> function
(define-function/types (->symbol arg1 arg2)
  [(string-expression? arg2) type:symbol])

; sql+quotable (U select (listof sql+quotable)) -> function
(define-function (in arg1 arg2)
  #:contract (-> sql+quotable? (or/c select? (listof sql+quotable?)) function?)
  (let* ([arg1  (sql-quote arg1)]
         [arg2  (or (select? arg2) (map sql-quote arg2))]
         [type1 (expression-type arg1)]
         [type2 (cond [(null? arg2)   type1] ; hack to make sure the type check passes
                      [(pair? arg2)   (let ([type2 (expression-type (car arg2))])
                                        (unless (andmap (cut type-compatible? type2 <>)
                                                        (map expression-type arg2))
                                          (error "all elements of list argument must have the same type"))
                                        type2)]
                      [(select? arg2) (let ([columns (select-what arg2)])
                                        (unless (= (length columns) 1)
                                          (error "select argument must have exactly one column"))
                                        (expression-type (car columns)))])])
    (unless (type-compatible? type1 type2)
      (error "argument types do not match"))
    (make-function type:boolean 'in (list arg1 arg2))))

; sql+quotable sql+quotable [sql+quotable] -> function
(define-function (sql-if test pos [neg undefined])
  (let ([test (sql-quote test)]
        [pos  (sql-quote pos)]
        [neg  (if (undefined? neg)
                  (sql-null (expression-type pos))
                  (sql-quote neg))])
    (make-function
     (type-rules
      [(and (boolean-expression? test) (andmap boolean-expression?   (list pos neg))) type:boolean]
      [(and (boolean-expression? test) (andmap integer-expression?   (list pos neg))) type:integer]
      [(and (boolean-expression? test) (andmap numeric-expression?   (list pos neg))) type:real]
      [(and (boolean-expression? test) (andmap symbol-expression?    (list pos neg))) type:symbol]
      [(and (boolean-expression? test) (andmap character-expression? (list pos neg))) type:string]
      [(and (boolean-expression? test) (andmap time-tai-expression?  (list pos neg))) type:time-tai]
      [(and (boolean-expression? test) (andmap temporal-expression?  (list pos neg))) type:time-utc])
     'if test pos neg)))

; (_ [expt expr] ... [else expr])
(define-syntax sql-cond
  (syntax-rules (else)
    [(_ [test expr1])
     (sql-if test expr1)]
    [(_ [test expr1] [else expr2])
     (sql-if test expr1 expr2)]
    [(_ [test1 expr1] [test2 expr2] ...)
     (sql-if test1 expr1 (sql-cond [test2 expr2] ...))]))

; Provide statements -----------------------------

; The define-function forms above expand to their own provide statements.

(provide sql-cond)
