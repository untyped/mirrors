#lang scheme/base

(require "../test-base.ss"
         "javascript.ss")

(define response-tests
  (test-suite "response.ss"
    
    (test-case "make-js-response"
      (check-pred response/full? (make-js-response (js)))
      (check-pred response/full? (make-js-response #:message #"OK" (js)))
      (check-pred response/full? (make-js-response #:message "OK" (js))))))

; Provide statements -----------------------------

(provide response-tests)