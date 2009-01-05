#lang scheme/base

(require scheme/serialize
         (only-in srfi/1 append-map)
         "../base.ss"
         "../plain/render.ss")

; Structure types --------------------------------

; (struct)
(define-struct csv () #:prefab)

; (struct (listof row))
(define-struct (sheet csv) (rows) #:prefab)

; (struct (listof cell))
(define-struct (row csv) (cells) #:prefab)

; (struct quotable-value)
(define-struct (cell csv) (value) #:prefab)

; Constructors ---------------------------------

; (U row (listof row))... -> csv
(define (create-sheet . rows)
  (make-sheet (normalize rows)))

; (U cell (listof cell)) ... -> row
(define (create-row . cells)
  (make-row (normalize cells)))

; quotable-value -> cell
(define create-cell
  make-cell)

; Helpers ------------------------------

; (listof (U any (listof any))) -> (listof any)
(define (normalize data)
  (cond [(null? data) null]
        [(pair? data) (append-map normalize data)]
        [else         (list data)]))

; Provide statements --------------------------- 

; contract
(define rows/c
  (flat-rec-contract row-or-row-list/c
    row? 
    (listof row-or-row-list/c)))

; contract
(define cells/c
  (flat-rec-contract cell-or-cell-list/c
    cell?
    (listof cell-or-cell-list/c)))

(provide/contract
 [struct csv         ()]
 [struct (sheet csv) ([rows (listof row?)])]
 [struct (row csv)   ([cells (listof cell?)])]
 [struct (cell csv)  ([value quotable-value?])]
 [create-sheet       (->* () () #:rest (listof rows/c) sheet?)]
 [create-row         (->* () () #:rest (listof cells/c) row?)]
 [create-cell        (-> quotable-value? cell?)]
 [quotable-value?    procedure?])
