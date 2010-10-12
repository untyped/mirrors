#lang scheme/base

(require "../test-base.ss"
         "xml.ss")

(define/provide-test-suite struct-tests
  
  (test-case "xml-quotable?"
    (check-true  (xml-quotable? "hi"))
    (check-false (xml-quotable? (xml "hi"))))
  
  (test-case "xml+quotable?"
    (check-true (xml+quotable? "hi"))
    (check-true (xml+quotable? (xml "hi"))))
  
  (test-case "xml-quote"
    (check-equal? (xml-quote #t) (xml* #t))
    (check-equal? (xml-quote #f) (xml* #f))
    (check-equal? (xml-quote "<dave>") (xml* "<dave>"))
    (check-equal? (xml-quote (xml)) (xml))
    (check-not-equal? (xml-quote "<dave>") (xml* (!raw "<dave>"))))
  
  (test-case "xml-empty?"
    (check-true (xml-empty? (xml)))
    (check-true (xml-empty? (xml #f)))
    (check-true (xml-empty? (xml ,(xml))))
    (check-true (xml-empty? (xml ,@(list (xml) (xml) (xml)))))
    (check-true (xml-empty? (xml (!raw ""))))))
