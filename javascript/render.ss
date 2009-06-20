#lang scheme/base

(require "../base.ss")

(require (javascript-in config print)
         (pprint-in)
         (unlib-in debug list profile)
         "quote.ss"
         "render-fast.ss"
         "struct.ss")

; Parameters -------------------------------------

; (parameter boolean)
(define render-pretty-javascript?
  (make-parameter #t))

; Public procedures ------------------------------

; javascript -> string
(define (javascript->string js)
  #;(parameterize ([allow-nested-function-declarations? #t]
                   [formatters/Expression               (list* format-FunctionExpression
                                                               format-RawExpression
                                                               (formatters/Expression))]
                   [formatters/Statement                (list* format-BeginStatement
                                                               (formatters/Statement))])
      (pretty-format (group (format-term js)) #f))
  (if (render-pretty-javascript?)
      (javascript->pretty-string js)
      (fast-javascript->string js)))


; javascript -> string
(define (javascript->pretty-string js)
  (parameterize ([allow-nested-function-declarations? #t]
                 [formatters/Expression               (list* format-FunctionExpression
                                                             format-RawExpression
                                                             (formatters/Expression))]
                 [formatters/Statement                (list* format-BeginStatement
                                                             (formatters/Statement))])
    (pretty-format (format-term js))))

; Custom printers --------------------------------

; FunctionExpression -> doc
(define format-FunctionExpression
  (match-lambda
    [(struct FunctionExpression (_ name args body))
     (h-append (text "function")
               (if name
                   (h-append (text " ")
                             (format-identifier name))
                   empty)
               (text "(")
               (h-concat (apply-infix (text ", ") (map format-identifier args)))
               (text ") {")
               (nest (current-indentation-width)
                     (format-map format-source-element body formatters/StatementList))
               line
               (text "}"))]))

; BeginStatement -> doc
(define format-BeginStatement
  (match-lambda
    [(struct BeginStatement (_ statements))
     (let ([statements (reverse (collect-begin-substatements statements))])
       (if (null? statements)
           (h-append)
           (h-append (format-substatement (car statements))
                     (format-map (lambda (statement)
                                   (h-append line (format-substatement statement)))
                                 (cdr statements)
                                 formatters/StatementList))))]))

; RawStatement -> doc
(define format-RawExpression
  (match-lambda
    [(struct RawExpression (_ str))
     (text str)]))

; (listof statement) [(listof doc)] -> (listof statement)
(define (collect-begin-substatements statements [accum null])
  (match statements
    [(list) accum]
    [(list-rest curr rest)
     (if (BeginStatement? curr)
         (collect-begin-substatements rest (collect-begin-substatements (BeginStatement-statements curr) accum))
         (collect-begin-substatements rest (cons curr accum)))]
    [other (debug "bad item" other)]))

; Provide statements -----------------------------

(provide/contract
 [render-pretty-javascript? (parameter/c boolean?)]
 [javascript->string        (-> javascript? string?)]
 [javascript->pretty-string (-> javascript? string?)])
