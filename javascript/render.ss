#lang scheme/base

(require scheme/contract
         scheme/match
         (planet dherman/javascript:8/print)
         (planet dherman/pprint:4)
         (only-in (planet schematics/schemeunit:3/util)
                  require/expose)
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
  (parameterize ([formatters/Statement
                  (cons format-BeginStatement
                        (formatters/Statement))])
    (pretty-format (format-term js))))

; Custom printers --------------------------------

; BeginStatement -> doc
(define (format-BeginStatement stmt)
  (match stmt
    [(struct BeginStatement (_ statements))
     (format-BeginStatement-substatements statements)]))

; (listof statement) -> doc
(define (format-BeginStatement-substatements statements)
  (cond [(null? statements)              (h-append)]
        [(empty-begin? (car statements)) (format-BeginStatement-substatements (cdr statements))]
        [(null? (cdr statements))        (format-substatement (car statements))]
        [else                            (h-append (format-substatement (car statements))
                                                   line
                                                   (format-BeginStatement-substatements (cdr statements)))]))

; Provide statements -----------------------------

(provide/contract
 [render-pretty-javascript? (parameter/c boolean?)]
 [javascript->string        (-> javascript? string?)]
 [javascript->pretty-string (-> javascript? string?)])
