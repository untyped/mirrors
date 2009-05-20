#lang scheme/base

(require "../base.ss")

(require (unlib-in profile)
         "render-internal.ss"
         "struct.ss")

(define-timer render-xml-timer)

; xml -> string
(define (xml->string xml)
  (with-timer render-xml-timer
    (define out (open-output-string))
    (display-xml xml out)
    (get-output-string out)))

; node output-port -> void
(define (display-xml xml out)
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
  (display-xml* xml out))

; atom output-port -> void
(define (display-atom atom out)
  (display-string (quotable-value->string (atom-data atom)) out))

; block output-stream -> void
(define (display-block block out)
  (for-each (cut display-xml <> out)
            (block-children block)))

; element output-stream -> void
(define (display-element elem out)
  (define tag        (element-tag elem))        ; symbol
  (define attributes (element-attributes elem)) ; (listof attribute)
  (define child      (element-child elem))      ; block
  (display #\< out)
  (display tag out)
  (unless (null? attributes)
    (display-attributes attributes out))
  (if (and (block? child) (null? (block-children child)) (not (preserve-singletons? tag)))
      (begin (display " />" out))
      (begin (display #\> out)
             (display-xml child out)
             (display "</" out)
             (display tag out)
             (display #\> out))))

; (alistof symbol any) output-stream -> void
(define (display-attributes attributes out)
  ; Don't escape double quotes:
  (for-each (lambda (attribute)
              (define name (attribute-name attribute))
              (define val  (atom-data (attribute-value attribute)))
              (display #\space out)
              (display name out)
              (display "=\"" out)
              (display-string (quotable-value->string val #f) out)
              (display #\" out))
            attributes))

; entity output-port -> void
(define (display-entity entity out)
  (define code (entity-code entity))
  (display #\& out)
  (when (integer? code)
    (display #\# out))
  (display code out)
  (display #\; out))

; comment output-stream -> void
(define (display-comment comment out)
  (display "<!--" out)
  (for-each (lambda (datum)
              (display (quotable-value->string datum) out))
            (comment-data comment))
  (display "-->" out))

; cdata output-stream -> void
(define (display-cdata cdata out)
  (display "<![CDATA[" out)
  (for-each (lambda (datum)
              (display (quotable-value->string datum) out))
            (cdata-data cdata))
  (display "]]>" out))

; pi output-stream -> void
(define (display-pi pi out)
  (display "<?" out)
  (for-each (lambda (datum)
              (display (quotable-value->string datum) out))
            (pi-data pi))
  (display "?>" out))

; raw output-port -> void
(define (display-raw raw out)
  (for-each (lambda (datum)
              (display (quotable-value->string datum) out))
            (raw-data raw)))

; Provide statements -----------------------------

(provide/contract 
 [xml->string (-> xml? string?)])
