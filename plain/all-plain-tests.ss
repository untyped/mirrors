#lang scheme/base

(require "../test-base.ss")

(require "response-test.ss")

(define/provide-test-suite all-plain-tests
  response-tests)
