#lang scheme/base

(require "../test-base.ss"
         "render-test.ss"
         "struct-test.ss")

; test-suite
(define all-csv-tests
  (test-suite "csv"
    struct-tests
    render-tests))

; Provide statements -----------------------------

(provide all-csv-tests)
