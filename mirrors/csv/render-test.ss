#lang scheme/base

(require "../test-base.ss"
         "render.ss"
         "struct.ss")

; Helpers ----------------------------------------

(define utc-date (date->time-utc (make-date 0 56 34 12 01 02 2003 0)))
(define tai-date (date->time-tai (make-date 0 56 34 12 01 02 2003 0)))

; Tests ------------------------------------------

; test-suite
(define render-tests
  (test-suite "render.ss"
    
    (test-case "atomic values"
      (check-equal? (csv->string (cell "string"))                 "\"string\"")
      (check-equal? (csv->string (cell "123"))                    "\"123\"")
      (check-equal? (csv->string (cell "0123"))                   "\"'0123\"")
      (check-equal? (csv->string (cell 'symbol))                  "\"symbol\"")
      (check-equal? (csv->string (cell 123.456))                  "123.456")
      (check-equal? (csv->string (cell #t))                       "yes")
      (check-equal? (csv->string (cell #f))                       "")
      (check-equal? (csv->string (cell #"bytes"))                 "\"bytes\"")
      (check-equal? (csv->string (cell (string->url "/u/r/l")))   "\"/u/r/l\"")
      (check-equal? (csv->string (cell "\"string\" with quotes")) "\"\"\"string\"\" with quotes\"")
      ; These checks give different results depending on your time zone and DST settings:
      (let ([hour (+ 12 (floor (/ (current-time-zone-offset) (* 60 60))))])
        (check-equal? (csv->string (cell utc-date)) (format "\"2003-02-01 ~a:34:56\"" hour))
        (check-equal? (csv->string (cell tai-date)) (format "\"2003-02-01 ~a:34:56\"" hour))))
    
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
