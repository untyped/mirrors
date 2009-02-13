#lang scheme/base

(require "../test-base.ss"
         "xml.ss")

(define response-tests
  (test-suite "response.ss"
    
    (test-case "make-xml-response"
      (check-pred response/full? (make-xml-response (xml)))
      (check-pred response/full? (make-xml-response #:message #"OK" (xml)))
      (check-pred response/full? (make-xml-response #:message "OK" (xml))))
    
    (test-case "make-html-response"
      (check-pred response/full? (make-html-response (xml)))
      (check-pred response/full? (make-html-response #:message #"OK" (xml)))
      (check-pred response/full? (make-html-response #:message "OK" (xml))))))

; Provide statements -----------------------------

(provide response-tests)