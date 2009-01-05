#lang scheme/base

(require (file "all-mirrors-tests.ss")
         (file "test-base.ss"))

(parameterize ([print-struct #t])
  (test/text-ui all-mirrors-tests))