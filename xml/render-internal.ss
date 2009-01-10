#lang scheme/base

(require net/url
         scheme/contract
         (file "../javascript/render.ss")
         (file "../javascript/struct.ss")
         (file "struct-internal.ss"))

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
  (and (memq tag '(script div p ul li span th td a iframe)) #t))

; (U attribute (listof attribute)) -> (U raw block)
(define (attributes->raw attr+attrs)
  (define (do-single-attribute attr)
    (define val (attribute-value attr))
    (make-raw (list (format " ~a=\"~a\""
                            (attribute-name attr)
                            (if (raw? val)
                                (raw-data val)
                                (let ([out (open-output-string)])
                                  (display-string (quotable-value->string (atom-data val) #f) out)
                                  (get-output-string out)))))))
  (if (attribute? attr+attrs)
      (do-single-attribute attr+attrs)
      (make-block (map do-single-attribute attr+attrs))))

; quotable-value -> quotable-value
(define (quote-javascript-attribute-value val)
  (if (javascript? val)
      (javascript->string val)
      val))

; string output-stream -> void
(define (display-string str out)
  (define len (string-length str))
  (let loop ([i 0])
    (when (< i len)
      (let ([char (string-ref str i)])
        (case char
          [(#\&) (display "&amp;" out)]
          [(#\") (display "&quot;" out)]
          [(#\<) (display "&lt;" out)]
          [(#\>) (display "&gt;" out)]
          [else  (display char out)]))
      (loop (add1 i)))))

; Provide statements -----------------------------

(provide/contract
 [quotable-value->string           (->* (quotable-value?) (boolean?) string?)]
 [preserve-singletons?             (-> symbol? boolean?)]
 [attributes->raw                  (-> (or/c attribute? (listof attribute?)) (or/c raw? block?))]
 [quote-javascript-attribute-value (-> quotable-value? quotable-value?)]
 [display-string                   (-> string? output-port? void?)])
