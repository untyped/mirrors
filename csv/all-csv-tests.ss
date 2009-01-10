#lang scheme/base

(require (file "../test-base.ss")
         (file "render-test.ss")
         (file "struct-test.ss"))

; test-suite
(define all-csv-tests
  (test-suite "csv"
    struct-tests
    render-tests))

; Provide statements -----------------------------

(provide all-csv-tests)