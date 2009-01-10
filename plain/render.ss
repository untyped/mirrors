#lang scheme/base

(require "../base.ss")

; quotable-value [boolean] -> string
(define (quotable-value->string val [pretty? #t])
  (cond [(string? val)     val]
        [(symbol? val)     (symbol->string val)]
        [(bytes? val)      (bytes->string/utf-8 val)]
        [(number? val)     (number->string val)]
        [(boolean? val)    (if val "yes" "")]
        [(url? val)        (url->string val)]
        [(time-utc? val)   (time-utc->string val)]
        [(time-tai? val)   (time-tai->string val)]))

; any -> boolean
(define (quotable-value? val)
  (or (string? val)
      (symbol? val)
      (bytes? val)
      (number? val)
      (boolean? val)
      (url? val)
      (time-utc? val)
      (time-tai? val)))

; Helpers ----------------------------------------

; time-utc -> string
(define (time-utc->string val)
  (define fmt (current-time-format))
  (if (string? fmt)
      (date->string (time-utc->date val) fmt)
      (fmt (time-utc->date val))))

; time-tai -> string
(define (time-tai->string val)
  (define fmt (current-time-format))
  (if (string? fmt)
      (date->string (time-tai->date val) fmt)
      (fmt (time-tai->date val))))

; Provide statements -----------------------------

(provide/contract
 [quotable-value->string           (->* (quotable-value?) (boolean?) string?)]
 [quotable-value?                  (-> any/c boolean?)]
 [time-utc->string                 (-> time-utc? string?)]
 [time-tai->string                 (-> time-tai? string?)])
 

       
