#lang scheme/base

(require "../base.ss"
         (prefix-in plain: "../plain/render.ss")
         "struct.ss")

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
  (display (quotable-value->string (cell-value cell)) out))

; Helpers --------------------------------------

; quotable-value [boolean] -> string
(define (quotable-value->string val [pretty? #t])
  (cond [(string? val)     (add-quotes (escape-string val))]
        [(symbol? val)     (add-quotes (escape-string (symbol->string val)))]
        [(bytes? val)      (add-quotes (escape-string (bytes->string/utf-8 val)))]
        [(number? val)     (number->string val)]
        [(boolean? val)    (if val "yes" "")]
        [(url? val)        (add-quotes (escape-string (url->string val)))]
        [(time-utc? val)   (add-quotes (escape-string (plain:time-utc->string val)))]
        [(time-tai? val)   (add-quotes (escape-string (plain:time-tai->string val)))]))

(define NEWLINE-REGEXP #rx"[\r\n]+")
(define QUOTE-REGEXP #rx"\"")

; string -> string
(define (escape-string value)
  (regexp-replace* QUOTE-REGEXP (regexp-replace* NEWLINE-REGEXP value " ") "\"\""))

; string -> string
(define (add-quotes val)
  (string-append "\"" val "\""))

; Provide statements --------------------------- 

(provide/contract
 [csv->string (-> csv? string?)])
