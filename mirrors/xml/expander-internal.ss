#lang scheme/base

(require "../base.ss")

; (struct (syntax -> syntax)
;         (syntax -> syntax)
;         syntax-certifier)
(define-struct xml-expander 
  (xml-xform expr-xform cert)
  #:transparent
  #:property prop:procedure 
  (struct-field-index expr-xform))

; Provide statements -----------------------------

; contract
(define syntax-certifier/c
  (->* (syntax?) (any/c (or/c procedure? false/c)) syntax?))

(provide/contract
 [struct xml-expander ([xml-xform  (-> syntax? syntax?)]
                          [expr-xform (-> syntax? syntax?)]
                          [cert       syntax-certifier/c])])
