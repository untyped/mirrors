#lang scheme/base

(require "../base.ss"
         "../javascript/render.ss"
         "../javascript/struct.ss"
         (prefix-in plain: "../plain/render.ss")
         "struct-internal.ss")

; quotable-value [boolean] -> string
(define (quotable-value->string val [pretty? #t])
  (cond [(plain:quotable-value? val) (plain:quotable-value->string val)]
        [(javascript? val)           (if pretty?
                                         (javascript->string val)
                                         (javascript->packed-string val))]
        [else              (error (format "Expected quotable value, received ~s." val))]))

; symbol -> boolean
(define (preserve-singletons? tag)
  (and (memq tag '(script div p ul li span table thead tbody tfoot tr th td a textarea canvas iframe)) #t))

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
      (javascript->packed-string val)
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
