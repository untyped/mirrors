#lang scheme/base

(require "csv/all-csv-tests.ss"
         "javascript/all-javascript-tests.ss"
         "plain/all-plain-tests.ss"
         "xml/all-xml-tests.ss"
         "test-base.ss")

; Tests ------------------------------------------

(define all-mirrors-tests
  (test-suite "mirrors"
    all-csv-tests
    all-javascript-tests
    all-plain-tests
    all-xml-tests))

; Provide statements -----------------------------

(provide all-mirrors-tests)
