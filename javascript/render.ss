#lang scheme/base

(require scheme/contract
         scheme/match
         (prefix-in s: scheme/pretty)
         (planet untyped/unlib/profile)
         (file "../javascript.plt-5.4-hacked/syntax/pretty-print.ss")
         (file "quote.ss")
         (file "render-fast.ss")
         (file "struct.ss"))

; Public procedures ------------------------------

; javascript -> string
(define (javascript->string js)
  (let ([js (quote-javascript js)])
    #;(fast-javascript->string js)
    (parameterize ([collapse-lines? #t])
      (javascript->pretty-string js))))

; javascript -> string
(define (javascript->pretty-string js)
  (let ([js (quote-javascript js)])
    #;(fast-javascript->string js)
    (parameterize ([pretty-printers/Statement (cons pretty-print-BeginStatement (pretty-printers/Statement))])
      (pretty-format js))))

; Custom printers --------------------------------

; BeginStatement -> void
(define (pretty-print-BeginStatement stmt)
  (match stmt
    [(struct BeginStatement (_ statements))
     (define tot (length statements))
     (define num 1)
     (for-each/X (lambda (statement)
                   (pretty-print-substatement statement)
                   (when (< num tot)
                     (pretty-newline))
                   (set! num (add1 num)))
                 statements
                 pretty-printers/StatementList)]))

; Provide statements -----------------------------

(provide/contract
 [javascript->string        (-> javascript? string?)]
 [javascript->pretty-string (-> javascript? string?)])