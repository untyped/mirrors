#lang scheme/base

(require net/url
         scheme/contract
         srfi/26/cut
         (file "../base.ss")
         (file "struct.ss"))

; Regular expressions --------------------------

(define NEWLINE-REGEXP #rx"[\r\n]")
(define QUOTE-REGEXP #rx"\"")

; Procedures -----------------------------------

; csv -> plain
(define (csv->string csv)
  (define out (open-output-string))
  (display-csv csv out)
  (get-output-string out))

; csv output-port -> void
(define (display-csv csv out)
  (cond [(sheet? csv) (display-sheet csv out)]
        [(row? csv)   (display-row csv out)]
        [(cell? csv)  (display-cell csv out)]))

; sheet output-port -> void
(define (display-sheet sheet out)
  (let loop ([rows (sheet-rows sheet)])
    (cond [(null? rows)       (void)]
          [(null? (cdr rows)) (display-row (car rows) out)]
          [else               (display-row (car rows) out)
                              (newline out)
                              (loop (cdr rows))])))

; row output-port -> void
(define (display-row row out)
  (let loop ([cells (row-cells row)])
    (cond [(null? cells)       (void)]
          [(null? (cdr cells)) (display-cell (car cells) out)]
          [else                (display-cell (car cells) out)
                               (display #\, out)
                               (loop (cdr cells))])))

; cell output-port -> void
(define (display-cell cell out)
  (define value (cell-value cell))
  (cond [(boolean? value) (if value
                              (display "yes" out)
                              (display "no" out))]
        [(number? value)  (display value out)]
        [(string? value)  (display #\" out)
                          (display (escape-string-value value) out)
                          (display #\" out)]
        [(symbol? value)  (display #\" out)
                          (display (escape-string-value (symbol->string value)) out)
                          (display #\" out)]
        [(bytes? value)   (display #\" out)
                          (display (escape-string-value (bytes->string/utf-8 value)) out)
                          (display #\" out)]
        [(url? value)     (display #\" out)
                          (display (escape-string-value (url->string value)) out)
                          (display #\" out)]
        [else (raise-exn exn:fail:contract
                (format "Expected quotable-value, received ~s." value))]))

; Helpers --------------------------------------

; string -> string
(define (escape-string-value value)
  (regexp-replace* QUOTE-REGEXP (regexp-replace* NEWLINE-REGEXP value " \\n ") "\\\\\""))

; Provide statements --------------------------- 

(provide/contract
 [csv->string (-> csv? string?)])
