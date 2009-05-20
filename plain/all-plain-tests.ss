#lang scheme/base

(require "../test-base.ss")

(require "response-test.ss")

(define all-plain-tests
  (test-suite "plain"
    response-tests))

; Provide statements -----------------------------

(provide all-plain-tests)
