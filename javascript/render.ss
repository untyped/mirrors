#lang scheme/base

(require "../base.ss")

(require (javascript-in config print)
         (pprint-in)
         (unlib-in debug list profile)
         "quote.ss"
         "render-fast.ss"
         "struct.ss")

; Parameters -------------------------------------

; (parameter (U 'pretty 'packed 'fast))
(define javascript-rendering-mode
  (make-parameter 'pretty))

; Public procedures ------------------------------

; javascript -> string
(define (javascript->string js)
  (case (javascript-rendering-mode)
    [(pretty) (parameterize ([formatters/Expression (list* format-FunctionExpression
                                                           format-RawExpression
                                                           (formatters/Expression))]
                             [formatters/Statement  (list* format-BeginStatement
                                                           (formatters/Statement))])
                (pretty-format (format-term js)))]
    [(packed) (parameterize ([formatters/Expression (list* format-FunctionExpression
                                                           format-RawExpression
                                                           (formatters/Expression))]
                             [formatters/Statement  (list* format-BeginStatement
                                                           (formatters/Statement))])
                (pretty-format (group (format-term js)) #f))]
    [(fast)   (fast-javascript->string js)]))

; javascript -> string
(define (javascript->packed-string js)
  (parameterize ([javascript-rendering-mode 'packed])
    (javascript->string js)))

; javascript -> string
(define (javascript->pretty-string js)
  (parameterize ([javascript-rendering-mode 'pretty])
    (javascript->string js)))

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
           (h-append (format-begin-substatement (car statements))
                     (format-map (lambda (statement)
                                   (h-append line (format-begin-substatement statement)))
                                 (cdr statements)
                                 formatters/StatementList))))]))

; (U Declaration Statement) -> doc
; Named function declarations aren't allowed inside regular statements in some
; JS VMs, so by default JS.plt disallows this arrangement using the contract on
; format-substatement. This is obviously no good for BeginStatements: the function
; below provides a workaround.
(define (format-begin-substatement stmt+decl)
  (if (Declaration? stmt+decl)
      (format-declaration stmt+decl)
      (format-statement stmt+decl)))

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
 [javascript-rendering-mode (parameter/c (or/c 'pretty 'packed 'fast))]
 [javascript->string        (-> javascript? string?)]
 [javascript->packed-string (-> javascript? string?)]
 [javascript->pretty-string (-> javascript? string?)])
