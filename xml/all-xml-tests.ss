#lang scheme/base

(require "../test-base.ss"
         "render-test.ss"
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
    util-tests))

; Provide statements -----------------------------

(provide all-xml-tests)
