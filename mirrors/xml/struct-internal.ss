#lang scheme/base

(require scheme/serialize
         "../base.ss"
         "../javascript/struct.ss"
         (prefix-in plain: "../plain/render.ss"))

; (struct)
(define-struct xml () #:prefab)

; (struct quotable-value)
(define-struct (atom xml) (data) #:prefab)

; (struct (listof xml))
(define-struct (block xml) (children) #:prefab)

; (struct (U integer symbol))
(define-struct (entity xml) (code) #:prefab)

; (struct (listof quotable-value))
(define-struct (comment xml) (data) #:prefab)

; (struct (listof quotable-value))
(define-struct (cdata xml) (data) #:prefab)

; (struct (listof quotable-value))
(define-struct (pi xml) (data) #:prefab)

; (struct (listof quotable-value))
(define-struct (raw xml) (data) #:prefab)

; (struct symbol (listof attribute) xml)
(define-struct (element xml) (tag attributes child) #:prefab)

; (struct symbol quotable-value)
(define-struct attribute (name value) #:prefab)

; any -> boolean
(define (quotable-value? val)
  (or (plain:quotable-value? val)
      (javascript? val)))

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
