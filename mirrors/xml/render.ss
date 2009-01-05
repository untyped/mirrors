#lang scheme/base

(require scheme/contract
         scheme/pretty
         srfi/26/cut
         (planet untyped/unlib/profile)
         (file "../base.ss")
         (file "render-internal.ss")
         (file "struct.ss"))

(define-timer render-xml-timer)

; (struct)
(define-struct render-seed () #:transparent)

; xml -> string
(define (xml->string xml)
  (with-timer render-xml-timer
    (define out (open-output-string))
    (display-xml xml out (make-render-seed))
    (get-output-string out)))

; node output-port render-seed -> void
(define (display-xml xml out seed)
  (define display-xml*
    (cond [(atom? xml)    display-atom]
          [(raw? xml)     display-raw]
          [(block? xml)   display-block]
          [(element? xml) display-element]
          [(entity? xml)  display-entity]
          [(comment? xml) display-comment]
          [(cdata? xml)   display-cdata]
          [(pi? xml)      display-pi]
          [else           (error (format "Expected xml, received ~s." xml))]))
  (display-xml* xml out seed))

; atom output-port render-seed -> void
(define (display-atom atom out seed)
  (display-string (quotable-value->string (atom-data atom)) out seed))

; block output-stream render-seed -> void
(define (display-block block out seed)
  (for-each (cut display-xml <> out seed)
            (block-children block)))

; element output-stream render-seed -> void
(define (display-element elem out seed)
  (define tag        (element-tag elem))        ; symbol
  (define attributes (element-attributes elem)) ; (listof attribute)
  (define child      (element-child elem))      ; block
  (display #\< out)
  (display tag out)
  (unless (null? attributes)
    (display-attributes attributes out seed))
  (if (and (block? child) (null? (block-children child)) (not (preserve-singletons? tag)))
      (begin (display " />" out))
      (begin (display #\> out)
             (display-xml child out seed)
             (display "</" out)
             (display tag out)
             (display #\> out))))

; (alistof symbol any) output-stream render-seed -> void
(define (display-attributes attributes out seed)
  ; Don't escape double quotes:
  (for-each (lambda (attribute)
              (define name (attribute-name attribute))
              (define val  (atom-data (attribute-value attribute)))
              (display #\space out)
              (display name out)
              (display "=\"" out)
              (display-string (quotable-value->string val #f) out seed)
              (display #\" out))
            attributes))

; entity output-port render-seed -> void
(define (display-entity entity out seed)
  (define code (entity-code entity))
  (display #\& out)
  (when (integer? code)
    (display #\# out))
  (display code out)
  (display #\; out))

; comment output-stream render-seed -> void
(define (display-comment comment out seed)
  (display "<!--" out)
  (for-each (lambda (datum)
              (display (quotable-value->string datum) out))
            (comment-data comment))
  (display "-->" out))

; cdata output-stream render-seed -> void
(define (display-cdata cdata out seed)
  (display "<![CDATA[" out)
  (for-each (lambda (datum)
              (display (quotable-value->string datum) out))
            (cdata-data cdata))
  (display "]]>" out))

; pi output-stream render-seed -> void
(define (display-pi pi out seed)
  (display "<?" out)
  (for-each (lambda (datum)
              (display (quotable-value->string datum) out))
            (pi-data pi))
  (display "?>" out))

; raw output-port render-seed -> void
(define (display-raw raw out seed)
  (for-each (lambda (datum)
              (display (quotable-value->string datum) out))
            (raw-data raw)))

; string output-stream render-seed -> void
(define (display-string str out seed)
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
 [xml->string (-> xml? string?)])