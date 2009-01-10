#lang scheme/base

(require scheme/class
         "../base.ss")

; Constants --------------------------------------

; symbol
(define postgresql8 'postgresql8)

; symbol
(define mysql5      'mysql5)

; symbol
(define sqlite3     'sqlite3)

; (listof symbol)
(define supported-dbmss
  (list postgresql8 mysql5 sqlite3))

; Interfaces -------------------------------------

(define table<%>
  (interface ()
    ))

(define column<%>
  (interface ()
    ))

; Procedures -------------------------------------

; any -> boolean
(define table?
  (cut is-a? <> table<%>))

; any -> boolean
(define column?
  (cut is-a? <> column<%>))

; Provide statements -----------------------------

(provide/contract
 [postgresql8     symbol?]
 [mysql5          symbol?]
 [sqlite3         symbol?]
 [supported-dbmss (listof symbol?)]
 [table<%>        interface?]
 [column<%>       interface?]
 [table?          procedure?]
 [column?         procedure?])
