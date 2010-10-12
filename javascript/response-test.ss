#lang scheme/base

(require "../test-base.ss"
         "../main.ss")

(define/provide-test-suite response-tests
    
    (test-case "make-js-response"
      (check-not-exn
        (cut make-js-response
             #:message   "message"
             #:mime-type "mime-type"
             (js (alert "Hi"))))
      (check-not-exn
        (cut make-js-response
             #:message   #"message"
             #:mime-type #"mime-type"
             (js (alert "Hi"))))))
