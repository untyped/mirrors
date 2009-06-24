#lang scheme/base

(require "../test-base.ss"
         "javascript-registry.ss"
         "javascript.ss")

(define/provide-test-suite javascript-registry-tests

  #:before (lambda () (registry-clear!))
  
  (test-case
   "registered script returned from registry"
   (after
    (define script (js (+ 1 2)))
    (registry-add! script)
    (check-equal? (registry->string) (javascript->string script))

    (registry-clear!)))

  (test-case
   "registered scripts returned in order of registering"
   (after
    (define s1 (js (+ 1 2)))
    (define s2 (js (/ 2 1)))
    (registry-add! s1)
    (registry-add! s2)
    (check-equal? (registry->string)
                  (string-append
                   (javascript->string s1)
                   "\n"
                   (javascript->string s2)))

    (registry-clear!)))
  )