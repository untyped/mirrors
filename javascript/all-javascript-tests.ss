#lang scheme/base

(require "../test-base.ss")

(require "op-test.ss"
         "quote-test.ss"
         "lang-test.ss"
         #;"render-test.ss"
         "response-test.ss"
         "syntax-test.ss")

(define all-javascript-tests
  (test-suite "javascript"
    op-tests
    quote-tests
    lang-tests
    syntax-tests
    #;render-tests
    response-tests))

; Provide statements -----------------------------

(provide all-javascript-tests)
