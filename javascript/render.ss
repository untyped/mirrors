#lang scheme/base

(require scheme/contract
         scheme/match
         (planet dherman/javascript:8/print)
         (planet dherman/pprint:4)
         (only-in (planet schematics/schemeunit:3/util)
                  require/expose)
         (planet untyped/unlib/debug)
         (planet untyped/unlib/list)
         (planet untyped/unlib/profile)
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
  (define quoted-js
    (quote-javascript js))
  (parameterize ([formatters/Statement
                  (cons format-BeginStatement
                        (formatters/Statement))])
    (pretty-format (group (format-term js)) 1000000000)))

; javascript -> string
(define (javascript->pretty-string js)
  (define quoted-js
    (quote-javascript js))
  (parameterize ([formatters/Statement (cons format-BeginStatement (formatters/Statement))])
    (pretty-format (format-term js))))

; Custom printers --------------------------------

; BeginStatement -> doc
(define (format-BeginStatement stmt)
  (match stmt
    [(struct BeginStatement (_ statements))
     (let ([statements (reverse (collect-begin-substatements statements))])
       (if (null? statements)
           (h-append)
           (h-append (format-substatement (car statements))
                     (format-map (lambda (statement)
                                   (h-append line (format-substatement statement)))
                                 (cdr statements)
                                 formatters/StatementList))))]))

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
