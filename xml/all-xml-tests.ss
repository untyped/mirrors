#lang scheme/base

(require "../test-base.ss")

(require "render-test.ss"
         "response-test.ss"
         "struct-test.ss"
         "syntax-expand-test.ss"
         "syntax-prerender-test.ss"
         "util-test.ss")

(define all-xml-tests
  (test-suite "xml"
    render-tests
    struct-tests
    syntax-expand-tests
    syntax-prerender-tests
    response-tests
    util-tests))

; Provide statements -----------------------------

(provide all-xml-tests)
