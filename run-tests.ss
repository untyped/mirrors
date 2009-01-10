#lang scheme/base

(require "all-mirrors-tests.ss"
         "test-base.ss")

(parameterize ([print-struct #t])
  (run-tests all-mirrors-tests))
