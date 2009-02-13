#lang scheme/base

(require "../test-base.ss"
         "response.ss")

(define response-tests
  (test-suite "response.ss"
    
    (test-case "make-plain-response"
      (check-pred response/full? (make-plain-response null))
      (check-pred response/full? (make-plain-response #:message #"OK" null))
      (check-pred response/full? (make-plain-response #:message "OK" null)))
    
    (test-case "make-plain-response/incremental"
      (check-pred response/incremental? (make-plain-response/incremental void))
      (check-pred response/incremental? (make-plain-response/incremental #:message #"OK" void))
      (check-pred response/incremental? (make-plain-response/incremental #:message "OK" void)))
    
    (test-case "make-redirect-response"
      (check-pred response/full? (make-redirect-response "http://www.example.com" ))
      (check-pred response/full? (make-redirect-response "http://www.example.com" #:message #"Moved"))
      (check-pred response/full? (make-redirect-response "http://www.example.com" #:message "Moved")))))

; Provide statements -----------------------------

(provide response-tests)