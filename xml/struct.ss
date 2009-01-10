#lang scheme/base

(require scheme/contract
         scheme/pretty)

(require (file "struct-private.ss"))

;; ((listof (U quotable-value xml)) -> block)
(define (create-block args)
  (make-block (map xml-quote args)))

;; (symbol (listof attribute) (U quotable-value xml) -> element)
(define (create-element tag attrs arg)
  (make-element tag attrs (xml-quote arg)))

;; (symbol (U atom raw quotable-value) -> element)
(define (create-attribute name val)
  (make-attribute name (xml-quote val)))

; quotable-value ... -> comment
(define (create-comment . args)
  (make-comment args))

; quotable-value ... -> cdata
(define (create-cdata . args)
  (make-cdata args))

; quotable-value ... -> pi
(define (create-pi . args)
  (make-pi args))

; quotable-value ... -> raw
(define (create-raw . args)
  (make-raw args))

;; (any -> boolean)
(define (quotable+xml? item)
  (or (xml? item)
      (quotable-value? item)))

;; ((U quotable-value xml) -> xml)
(define (xml-quote item)
  (cond [(xml? item)            item]
        [(quotable-value? item) (make-atom item)]
        [else                   (error (format "Expected (U quotable xml), received ~s" item))]))

;; ((opt-listof (U xml attribute)) -> integer)
(define (xml-size item)
  (cond [(block? item)     (add1 (xml-size (block-children item)))]
        [(element? item)   (add1 (+ (xml-size (element-attributes item)) (xml-size (element-child item))))]
        [(attribute? item) (add1 (xml-size (attribute-value item)))]
        [(list? item)      (apply + 0 (map xml-size item))]
        [else              1]))

;; ((opt-listof (U xml attribute)) -> integer)
(define (xml-raw-size item)
  (cond [(block? item)     (xml-raw-size (block-children item))]
        [(element? item)   (+ (xml-raw-size (element-attributes item)) (xml-raw-size (element-child item)))]
        [(attribute? item) (xml-raw-size (attribute-value item))]
        [(list? item)      (apply + 0 (map xml-raw-size item))]
        [(raw? item)       1]
        [else              0]))

;; ((opt-listof (U xml attribute)) -> integer)
(define (xml-depth item)
  (cond [(block? item)     (add1 (xml-depth (block-children item)))]
        [(element? item)   (add1 (max (xml-depth (element-attributes item)) (xml-depth (element-child item))))]
        [(attribute? item) (add1 (xml-depth (attribute-value item)))]
        [(list? item)      (apply max 0 (map xml-depth item))]
        [else              1]))

;; (xml -> void)
(define (xml-dump item)
  (define size (xml-size item))
  (define raw-size (xml-raw-size item))
  (define depth (xml-depth item))
  (define raw-percent (exact->inexact (floor (* (/ raw-size size) 100))))
  (printf "----- Size ~s, depth ~s, ~s% raw -----~n" size depth raw-percent)
  (pretty-print item))

; Provide statements --------------------------- 

(provide (except-out (struct-out xml) xml make-xml)
         (except-out (struct-out block) make-block)
         (except-out (struct-out element) make-element)
         (struct-out entity)
         (except-out (struct-out comment) make-comment)
         (except-out (struct-out cdata) make-cdata)
         (except-out (struct-out pi) make-pi)
         (except-out (struct-out raw) make-raw)
         (struct-out atom)
         (except-out (struct-out attribute) make-attribute))

(provide xml-size xml-depth xml-dump)

(provide/contract
 [rename create-block make-block         (-> (listof quotable+xml?) block?)]
 [rename create-element make-element     (-> symbol? (listof attribute?) quotable+xml? element?)]
 [rename create-attribute make-attribute (-> symbol? (or/c atom? raw? quotable-value?) attribute?)]
 [rename create-comment make-comment     (->* () () #:rest (listof quotable-value?) comment?)]
 [rename create-cdata make-cdata         (->* () () #:rest (listof quotable-value?) cdata?)]
 [rename create-pi make-pi               (->* () () #:rest (listof quotable-value?) pi?)]
 [rename create-raw make-raw             (->* () () #:rest (listof quotable-value?) raw?)])
