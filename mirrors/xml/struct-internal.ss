#lang scheme/base

(require net/url
         scheme/contract
         (file "../javascript/struct.ss"))

;; (struct)
(define-struct xml () #:transparent)

;; (struct quotable-value)
(define-struct (atom xml) (data) #:transparent)

;; (struct (listof xml))
(define-struct (block xml) (children) #:transparent)

;; (struct (U integer symbol))
(define-struct (entity xml) (code) #:transparent)

;; (struct (listof quotable-value))
(define-struct (comment xml) (data) #:transparent)

;; (struct (listof quotable-value))
(define-struct (cdata xml) (data) #:transparent)

;; (struct (listof quotable-value))
(define-struct (pi xml) (data) #:transparent)

;; (struct (listof quotable-value))
(define-struct (raw xml) (data) #:transparent)

;; (struct symbol (listof attribute) xml)
(define-struct (element xml) (tag attributes child) #:transparent)

;; (struct symbol quotable-value)
(define-struct attribute (name value) #:transparent)

;; (any -> boolean)
(define (quotable-value? item)
  (or (boolean? item)
      (number? item)
      (string? item)
      (symbol? item)
      (bytes? item)
      (url? item)
      (javascript? item)))

; Provide statements --------------------------- 

(provide quotable-value?)

(provide/contract
 [struct xml           ()]
 [struct (block xml)   ([children (listof xml?)])]
 [struct (element xml) ([tag symbol?] [attributes (listof attribute?)] [child xml?])]
 [struct (entity xml)  ([code (or/c symbol? integer?)])]
 [struct (comment xml) ([data (listof quotable-value?)])]
 [struct (cdata xml)   ([data (listof quotable-value?)])]
 [struct (pi xml)      ([data (listof quotable-value?)])]
 [struct (raw xml)     ([data (listof quotable-value?)])]
 [struct (atom xml)    ([data quotable-value?])]
 [struct attribute     ([name symbol?] [value (or/c atom? raw?)])])
