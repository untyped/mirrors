#lang scheme/base

(require (planet untyped/unlib:3/number)
         (planet untyped/unlib:3/symbol)
         (planet untyped/unlib:3/time)
         "../../base.ss")

; Structure types --------------------------------

(define-struct type                            ()             #:prefab)
(define-struct (boolean-type   type)           ()             #:prefab)
(define-struct (numeric-type   type)           ()             #:prefab)
(define-struct (integer-type   numeric-type)   ()             #:prefab)
(define-struct (real-type      numeric-type)   ()             #:prefab)
(define-struct (character-type type)           (max-length)   #:prefab)
(define-struct (symbol-type    character-type) ()             #:prefab)
(define-struct (string-type    character-type) ()             #:prefab)
(define-struct (temporal-type  type)           ()             #:prefab)
(define-struct (time-utc-type  temporal-type)  ()             #:prefab)
(define-struct (time-tai-type  temporal-type)  ()             #:prefab)

; Procedures -------------------------------------

; type any -> boolean
(define (type-valid? type val)
  (match type
    [(struct boolean-type ())    (or (boolean? val) (void? val))]
    [(struct integer-type ())    (or (integer? val) (not val))]
    [(struct real-type    ())    (or (real? val) (not val))]
    [(struct string-type  (len)) (or (and (string? val)
                                          (or (not len)
                                              (<= (string-length val) len)))
                                     (not val))]
    [(struct symbol-type  (len)) (or (and (symbol? val)
                                          (or (not len)
                                              (<= (symbol-length val) len)))
                                     (not val))]
    [(struct time-utc-type ())   (or (time-utc? val) (not val))]
    [(struct time-tai-type ())   (or (time-tai? val) (not val))]))

; type any -> boolean
(define (type-null? type val)
  (match type
    [(? boolean-type?)  (void? val)]
    [(? integer-type?)  (not val)]
    [(? real-type?)     (not val)]
    [(? string-type?)   (not val)]
    [(? symbol-type?)   (not val)]
    [(? time-utc-type?) (not val)]
    [(? time-tai-type?) (not val)]))

; type -> any
(define (type-null type)
  (match type
    [(? boolean-type?)  (void)]
    [(? integer-type?)  #f]
    [(? real-type?)     #f]
    [(? string-type?)   #f]
    [(? symbol-type?)   #f]
    [(? time-utc-type?) #f]
    [(? time-tai-type?) #f]))

; type type -> boolean
(define (type-compatible? type1 type2)
  (or (and (boolean-type? type1)   (boolean-type? type2))
      (and (numeric-type? type1)   (numeric-type? type2))
      (and (character-type? type1) (character-type? type2))
      (and (temporal-type? type1)  (temporal-type? type2))))

; Provide statements -----------------------------

(provide/contract
 [struct type           ()]
 [struct boolean-type   ()]
 [struct numeric-type   ()]
 [struct integer-type   ()]
 [struct real-type      ()]
 [struct character-type ([max-length (or/c natural? false/c)])]
 [struct string-type    ([max-length (or/c natural? false/c)])]
 [struct symbol-type    ([max-length (or/c natural? false/c)])]
 [struct temporal-type  ()]
 [struct time-utc-type  ()]
 [struct time-tai-type  ()]
 [type-valid?           (-> type? any/c boolean?)]
 [type-null?            (-> type? any/c boolean?)]
 [type-null             (-> type? any)]
 [type-compatible?      (-> type? type? boolean?)])
