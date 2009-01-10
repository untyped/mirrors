#lang scheme/base

(require net/url
         scheme/contract
         (file "../javascript/render.ss")
         (file "../javascript/struct.ss")
         (file "struct-private.ss"))

; quotable-value [boolean] -> string
(define (quotable-value->string val [pretty? #t])
  (cond [(string? val)     val]
        [(symbol? val)     (symbol->string val)]
        [(bytes? val)      (bytes->string/utf-8 val)]
        [(number? val)     (number->string val)]
        [(boolean? val)    (if val "yes" "no")]
        [(url? val)        (url->string val)]
        [(javascript? val) (if pretty?
                               (javascript->pretty-string val)
                               (javascript->string val))]
        [else              (error (format "Expected val, received ~s." val))]))

; symbol -> boolean
(define (preserve-singletons? tag)
  (if (memq tag '(script div p ul li span th td a iframe))
      #t
      #f))

; attribute -> raw
(define (attribute->raw attr)
  (make-raw (list (render display-attribute attr))))

; quotable-value -> quotable-value
(define (quote-javascript-attribute-value val)
  (if (javascript? val)
      (javascript->string val)
      val))

; Helpers ----------------------------------------

; (any output-port -> void) any -> string
(define (render display-proc arg)
  (let ([out (open-output-string)])
    (display-proc arg out)
    (get-output-string out)))

; attribute output-port -> void
(define (display-attribute attr out)
  (define name (attribute-name attr))
  (define val (attribute-value attr))
  (display #\space out)
  (display name out)
  (display "=\"" out)
  (if (raw? val)
      (display (raw-data val) out)
      (display-quotable-value (atom-data val) out))
  (display #\" out))

; quotable-value output-port -> void
(define (display-quotable-value val out)
  (cond [(boolean? val) (display (if val "true" "false") out)]
        [(number? val)  (display val out)]
        [(string? val)  (display-quoted-string val out)]
        [(symbol? val)  (display-quoted-string (symbol->string val) out)]
        [(bytes? val)   (display-quoted-string (bytes->string/utf-8 val) out)]
        [(url? val)     (display-quoted-string (url->string val) out)]
        [else           (error (format "Expected (U boolean number string symbol bytes), received ~s" val))]))

; string output-port -> void
(define (display-quoted-string val out)
  (let ([len (string-length val)])
    (let loop ([i 0])
      (when (< i len)
        (let ([char (string-ref val i)])
          (cond [(eq? char #\&) (display "&amp;" out)]
                [(eq? char #\") (display "&quot;" out)]
                [(eq? char #\<) (display "&lt;" out)]
                [(eq? char #\>) (display "&gt;" out)]
                [else           (display char out)])
          (loop (add1 i)))))))

; Provide statements -----------------------------

(provide/contract
 [quotable-value->string           (->* (quotable-value?) (boolean?) string?)]
 [preserve-singletons?             (-> symbol? boolean?)]
 [attribute->raw                   (-> attribute? raw?)]
 [quote-javascript-attribute-value (-> quotable-value? quotable-value?)])
