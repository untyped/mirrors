#lang scheme/base

(require "../test-base.ss")

(require "op-test.ss"
         "quote-test.ss"
         "lang-test.ss"
         #;"render-test.ss"
         "response-test.ss"
         "syntax-test.ss"
         "javascript-registry-test.ss"
         "sexp/module-test.ss")

(define/provide-test-suite all-javascript-tests
  op-tests
  quote-tests
  lang-tests
  syntax-tests
  #;render-tests
  response-tests
  javascript-registry-tests
  module-tests)
