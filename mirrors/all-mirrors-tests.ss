#lang scheme/base

(require (file "csv/all-csv-tests.ss")
         (file "javascript/all-javascript-tests.ss")
         (file "xml/all-xml-tests.ss")
         (file "test-base.ss"))

; Tests ------------------------------------------

(define all-mirrors-tests
  (test-suite "mirrors"
    all-csv-tests
    all-javascript-tests
    all-xml-tests))

; Provide statements -----------------------------

(provide all-mirrors-tests)