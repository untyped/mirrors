#lang scheme/base

(require "../test-base.ss"
         "response-test.ss")

; test-suite
(define all-plain-tests
  (test-suite "plain"
    response-tests))

; Provide statements -----------------------------

(provide all-plain-tests)
