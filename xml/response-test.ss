#lang scheme/base

(require "../test-base.ss"
         "../main.ss")

(define/provide-test-suite response-tests
  
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
           (xml "Hi")))))
