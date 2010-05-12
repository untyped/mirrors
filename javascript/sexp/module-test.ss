#lang scheme/base

(require scheme/match
         scheme/runtime-path
         "../../test-base.ss"
         "../javascript.ss"
         "../javascript-registry.ss")

(define-runtime-path here ".")
(define t1:script #f)

(define/provide-test-suite module-tests

  ;; Dynamically requiring the test1.ss module here makes
  ;; this test immune to the effects of other tests that may
  ;; play around with the registry. If we statically
  ;; required test1.ss, another test suite might clear the
  ;; registry before we get run.
  #:before (lambda ()
             (registry-clear!)
             (set! t1:script (dynamic-require (build-path here "test1.ss") 'script)))
  
  (test-case
   "module exports correct script binding"
   (check-equal? t1:script
                 (js (function dave (a b) (+ a b))
                     (function noel (a b) (/ a b)))))

  (test-case
   "registry contains module's script"
   (check
    regexp-match?
    (regexp-quote (javascript->string t1:script))
    (registry->string)))

  (test-case
   "registry contains dependency's script"
   (match (regexp-match-positions
           (regexp-quote (javascript->string t1:script))
           (registry->string))
          [(list (cons start end))
           (check > start 0)]
          [_ (fail "Registry does not contain test1.ss script")])))