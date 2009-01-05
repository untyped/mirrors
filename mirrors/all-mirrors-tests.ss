#lang scheme/base

(require "csv/all-csv-tests.ss"
         "javascript/all-javascript-tests.ss"
         "xml/all-xml-tests.ss"
         "test-base.ss")

; Tests ------------------------------------------

(define all-mirrors-tests
  (test-suite "mirrors"
    all-csv-tests
    all-javascript-tests
    all-xml-tests))

; Provide statements -----------------------------

(provide all-mirrors-tests)
