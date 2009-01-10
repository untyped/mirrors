#lang scheme/base

(require (file "../test-base.ss")
         (file "op-test.ss")
         (file "quote-test.ss")
         (file "lang-test.ss")
         #;(file "render-test.ss")
         (file "syntax-test.ss"))

(define all-javascript-tests
  (test-suite "javascript"
    op-tests
    quote-tests
    lang-tests
    syntax-tests
    #;render-tests))

; Provide statements -----------------------------

(provide all-javascript-tests)