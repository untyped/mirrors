#lang scheme/base

(require "test-base.ss")

(require "csv/all-csv-tests.ss"
         "javascript/all-javascript-tests.ss"
         "plain/all-plain-tests.ss"
         "xml/all-xml-tests.ss")

; Tests ------------------------------------------

(define all-mirrors-tests
  (test-suite "mirrors"
    all-plain-tests
    all-csv-tests
    all-javascript-tests
    all-xml-tests))

; Provide statements -----------------------------

(provide all-mirrors-tests)
