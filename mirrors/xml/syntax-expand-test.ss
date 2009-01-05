#lang scheme/base

(require "../test-base.ss"
         "struct.ss"
         "syntax.ss"
         "syntax-expand.ss")

; Helpers ----------------------------------------

(define test-url "http://www.example.com")
(define text     "Text")
(define sym      'symbol)
(define utc-date (date->time-utc (make-date 0 56 34 12 01 02 2003 0)))
(define tai-date (date->time-tai (make-date 0 56 34 12 01 02 2003 0)))

; Tests ------------------------------------------

(define syntax-expand-tests
  (test-suite "syntax-expand.ss"
    
    (test-case "xml*: literals"
      (check-equal? (xml* #t)           (make-atom #t)           "true")
      (check-equal? (xml* #f)           (make-atom #f)           "false")
      (check-equal? (xml* 12345)        (make-atom 12345)        "number")
      (check-equal? (xml* "blah &\"<>") (make-atom "blah &\"<>") "string")
      (check-equal? (xml* 'blah\&\"<>)  (make-atom 'blah\&\"<>)  "symbol")
      (check-equal? (xml* #"blah&\"<>") (make-atom #"blah&\"<>") "bytes")
      (check-equal? (xml* ,utc-date)    (make-atom utc-date)     "time-utc")
      (check-equal? (xml* ,tai-date)    (make-atom tai-date)     "time-tai"))
    
    (test-case "xml*: raw"
      (check-equal? (xml* (!raw "&\"<>"))   (make-raw "&\"<>")                  "string")
      (check-equal? (xml* (!raw 'dave))     (make-raw 'dave)                    "symbol")
      (check-equal? (xml* (!raw ,test-url)) (make-raw "http://www.example.com") "unquote"))
    
    (test-case "xml*: comment"
      (check-equal? (xml* (!comment "&\"<>"))   (make-comment "&\"<>")                  "string")
      (check-equal? (xml* (!comment 'dave))     (make-comment 'dave)                    "symbol")
      (check-equal? (xml* (!comment ,test-url)) (make-comment "http://www.example.com") "unquote"))
    
    (test-case "xml*: cdata"
      (check-equal? (xml* (!cdata "&\"<>"))   (make-cdata "&\"<>")                  "string")
      (check-equal? (xml* (!cdata 'dave))     (make-cdata 'dave)                    "symbol")
      (check-equal? (xml* (!cdata ,test-url)) (make-cdata "http://www.example.com") "unquote"))
    
    (test-case "xml*: pi"
      (check-equal? (xml* (!pi "&\"<>"))   (make-pi "&\"<>")                  "string")
      (check-equal? (xml* (!pi 'dave))     (make-pi 'dave)                    "symbol")
      (check-equal? (xml* (!pi ,test-url)) (make-pi "http://www.example.com") "unquote"))
    
    (test-case "xml*: entities"
      (check-equal? (xml* (& nbsp)) (make-entity 'nbsp)   "symbol")
      (check-equal? (xml* (& 1234)) (make-entity 1234)    "integer")
      (check-equal? (xml* (& ,sym)) (make-entity 'symbol) "unquote"))
    
    (test-case "xml*: elements"
      (check-equal? (xml* (br)) 
                    (make-element 'br null (make-block null))
                    "empty")
      (check-equal? (xml* (h1 "Dave"))
                    (make-element 'h1 null (make-atom "Dave"))
                    "children")
      (check-equal? (xml* (hr (@ [class "narrow"])))
                    (make-element 'hr (list (make-attribute 'class (make-atom "narrow"))) (make-block null))
                    "attributes")
      (check-equal? (xml* (span (@ [title "&\"<>"]) "stuff")) 
                    (make-element 'span (list (make-attribute 'title (make-atom "&\"<>"))) (make-atom "stuff"))
                    "attributes and children")
      (check-equal? (xml* (a (@ ,(make-attribute 'href test-url) [class ,text]) ,text)) 
                    (make-element 'a (list (make-attribute 'href test-url) (make-attribute 'class text)) (make-atom text))
                    "unquote in attributes and children")
      (check-equal? (xml* (a (@ ,@(list (make-attribute 'href test-url) (make-attribute 'class "blue")))
                            ,@(list text text)))
                    (make-element 'a 
                                  (list (make-attribute 'href test-url) (make-attribute 'class "blue"))
                                  (make-block (list (make-atom text) (make-atom text))))
                    "unquote-splicing in attributes and children"))
    
    (test-case "nested elements"
      (check-equal? (xml (a (b (c))))
                    (make-raw "<a><b><c /></b></a>")
                    "single children"))
    
    (test-equal? "top level unquote"
      (xml* ,"stuff")
      (make-atom "stuff"))))

; Provide statements -----------------------------

(provide syntax-expand-tests)
