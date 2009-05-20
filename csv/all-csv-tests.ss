#lang scheme/base

(require "../test-base.ss")

(require "render-test.ss"
         "response-test.ss"
         "struct-test.ss")

; test-suite
(define all-csv-tests
  (test-suite "csv"
    struct-tests
    render-tests
    response-tests))

; Provide statements -----------------------------

(provide all-csv-tests)
