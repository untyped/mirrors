#lang scheme/base

(require "../test-base.ss"
         "../main.ss")

(define response-tests
  (test-suite "response.ss"
    
    (test-case "make-csv-response"
      (check-not-exn
        (cut make-csv-response
             #:message   "message"
             #:mime-type "mime-type"
             (csv:sheet (csv:row (csv:cell "Hi")))))
      (check-not-exn
        (cut make-csv-response
             #:message   #"message"
             #:mime-type #"mime-type"
             (csv:sheet (csv:row (csv:cell "Hi"))))))))

; Provide statements -----------------------------

(provide response-tests)