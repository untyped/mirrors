#lang scheme/base

(require "../test-base.ss"
         "../main.ss")

(define response-tests
  (test-suite "response.ss"
    
    (test-case "make-xml-response"
      (check-not-exn
        (cut make-xml-response
             #:message   "message"
             #:mime-type "mime-type"
             (xml "Hi")))
      (check-not-exn
        (cut make-xml-response
             #:message   #"message"
             #:mime-type #"mime-type"
             (xml "Hi"))))
    
    (test-case "make-html-response"
      (check-not-exn
        (cut make-html-response
             #:message   "message"
             #:mime-type "mime-type"
             (xml "Hi")))
      (check-not-exn
        (cut make-html-response
             #:message   #"message"
             #:mime-type #"mime-type"
             (xml "Hi"))))))

; Provide statements -----------------------------

(provide response-tests)