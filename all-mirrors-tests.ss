#lang scheme/base

(require "test-base.ss")

(require "csv/all-csv-tests.ss"
         "javascript/all-javascript-tests.ss"
         "plain/all-plain-tests.ss"
         "xml/all-xml-tests.ss")

; Tests ------------------------------------------

(define/provide-test-suite all-mirrors-tests
  all-plain-tests
  all-csv-tests
  all-javascript-tests
  all-xml-tests)
