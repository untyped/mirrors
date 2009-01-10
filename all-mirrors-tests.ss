#lang scheme/base

(require (file "javascript/all-javascript-tests.ss")
         (file "xml/all-xml-tests.ss")
         (file "test-base.ss"))

; Tests ------------------------------------------

(define all-mirrors-tests
  (test-suite "mirrors"
    all-javascript-tests
    all-xml-tests))

; Provide statements -----------------------------

(provide all-mirrors-tests)