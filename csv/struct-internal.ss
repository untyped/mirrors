#lang scheme/base

(require net/url
         scheme/contract
         (only-in srfi/1/list append-map))

; Structure types --------------------------------

; (struct)
(define-struct csv () #:transparent)

; (struct (listof row))
(define-struct (sheet csv) (rows) #:transparent)

; (struct (listof cell))
(define-struct (row csv) (cells) #:transparent)

; (struct quotable-value)
(define-struct (cell csv) (value) #:transparent)

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

; any -> boolean
(define (quotable-value? val)
  (or (boolean? val)
      (number? val)
      (string? val)
      (symbol? val)
      (bytes? val)
      (url? val)))

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
