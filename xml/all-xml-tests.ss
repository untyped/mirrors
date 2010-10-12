#lang scheme/base

(require "../test-base.ss")

(require "render-test.ss"
         "response-test.ss"
         "struct-test.ss"
         "syntax-expand-test.ss"
         "syntax-prerender-test.ss"
         "util-test.ss")

(define/provide-test-suite all-xml-tests
  render-tests
  struct-tests
  syntax-expand-tests
  syntax-prerender-tests
  response-tests
  util-tests)