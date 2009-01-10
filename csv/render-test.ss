#lang scheme/base

(require net/url
         (file "../test-base.ss")
         (file "render.ss")
         (file "struct.ss"))

; Tests ----------------------------------------

; test-suite
(define render-tests
  (test-suite "render.ss"
    
    (test-case "atomic values"
      (check-equal? (csv->string (cell "string"))                 "\"string\"")
      (check-equal? (csv->string (cell 'symbol))                  "\"symbol\"")
      (check-equal? (csv->string (cell 123.456))                  "123.456")
      (check-equal? (csv->string (cell #t))                       "yes")
      (check-equal? (csv->string (cell #f))                       "no")
      (check-equal? (csv->string (cell #"bytes"))                 "\"bytes\"")
      (check-equal? (csv->string (cell (string->url "/u/r/l")))   "\"/u/r/l\"")
      (check-equal? (csv->string (cell "\"string\" with quotes")) "\"\\\"string\\\" with quotes\""))
    
    (test-equal? "single row"
      (csv->string (row (cell 1) (cell "2") (cell 3)))
      "1,\"2\",3")
    
    (test-equal? "multiple rows"
      (csv->string (sheet (row (cell 1) (cell 2) (cell 3))
                          (row (cell 4) (cell 5) (cell 6))
                          (row (cell 7) (cell 8) (cell 9))))
      (format "1,2,3~n4,5,6~n7,8,9"))))

; Provide statements ---------------------------

(provide render-tests)
