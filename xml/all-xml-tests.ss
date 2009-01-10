#lang scheme/base

(require (file "../test-base.ss")
         (file "render-test.ss")
         (file "syntax-expand-test.ss")
         (file "syntax-prerender-test.ss")
         (file "util-test.ss"))

(define all-xml-tests
  (test-suite "xml"
    render-tests
    syntax-expand-tests
    syntax-prerender-tests
    util-tests))

; Provide statements -----------------------------

(provide all-xml-tests)