#lang scheme/base

(require "../test-base.ss"
         "csv.ss")

(define response-tests
  (test-suite "response.ss"
    
    (test-case "make-csv-response"
      (check-pred response/full? (make-csv-response (csv:sheet)))
      (check-pred response/full? (make-csv-response #:message #"OK" (csv:sheet)))
      (check-pred response/full? (make-csv-response #:message "OK" (csv:sheet))))))

; Provide statements -----------------------------

(provide response-tests)