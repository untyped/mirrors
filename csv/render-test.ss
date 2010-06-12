#lang scheme/base

(require "../test-base.ss")

(require "render.ss"
         "struct.ss")

; Helpers ----------------------------------------

; GMT:
(define utc-winter-date (date->time-utc (make-date 0 56 34 12 01 02 2003)))
(define tai-winter-date (date->time-tai (make-date 0 56 34 12 01 02 2003)))

; BST:
(define utc-summer-date (date->time-utc (make-date 0 56 34 12 01 07 2003)))
(define tai-summer-date (date->time-tai (make-date 0 56 34 12 01 07 2003)))

; Tests ------------------------------------------

; test-suite
(define render-tests
  (test-suite "render.ss"
    
    (test-case "atomic values"
      (check-equal? (csv->string (cell "string"))                 "\"string\"")
      (check-equal? (csv->string (cell "123"))                    "\"123\"")
      (check-equal? (csv->string (cell "0123"))                   "\"0123\"")
      (check-equal? (csv->string (cell 'symbol))                  "\"symbol\"")
      (check-equal? (csv->string (cell 123.456))                  "123.456")
      (check-equal? (csv->string (cell #t))                       "yes")
      (check-equal? (csv->string (cell #f))                       "")
      (check-equal? (csv->string (cell #"bytes"))                 "\"bytes\"")
      (check-equal? (csv->string (cell (string->url "/u/r/l")))   "\"/u/r/l\"")
      (check-equal? (csv->string (cell "\"string\" with quotes")) "\"\"\"string\"\" with quotes\"")
      ; Times are rendered in the correct immediate time zone:
      (check-equal? (csv->string (cell utc-winter-date)) "\"2003-02-01 12:34:56\"" "time-utc (GMT)")
      (check-equal? (csv->string (cell tai-winter-date)) "\"2003-02-01 12:34:56\"" "time-utc (GMT)")
      (check-equal? (csv->string (cell utc-summer-date)) "\"2003-07-01 12:34:56\"" "time-utc (BST)")
      (check-equal? (csv->string (cell tai-summer-date)) "\"2003-07-01 12:34:56\"" "time-utc (BST)")
      (parameterize ([current-tz "PST8PDT"])
        (check-equal? (csv->string (cell utc-winter-date)) "\"2003-02-01 04:34:56\"" "time-utc (PST)")
        (check-equal? (csv->string (cell tai-winter-date)) "\"2003-02-01 04:34:56\"" "time-utc (PST)")
        (check-equal? (csv->string (cell utc-summer-date)) "\"2003-07-01 04:34:56\"" "time-utc (PDT)")
        (check-equal? (csv->string (cell tai-summer-date)) "\"2003-07-01 04:34:56\"" "time-utc (PDT)")))
    
    (test-equal? "single row"
      (csv->string (row (cell 1) (cell "2") (cell 3)))
      "1,\"2\",3")
    
    (test-equal? "multiple rows"
      (csv->string (sheet (row (cell 1) (cell 2) (cell 3))
                          (row (cell 4) (cell 5) (cell 6))
                          (row (cell 7) (cell 8) (cell 9))))
      (format "1,2,3~n4,5,6~n7,8,9"))))

; Provide statements -----------------------------

(provide render-tests)
