#lang scheme/base

(require "../test-base.ss")

(require "struct.ss"
         "syntax.ss")

; Helpers ----------------------------------------

(define test-url "http://www.example.com")
(define text     "Text")
(define sym      'symbol)
(define utc-date (date->time-utc (make-date 0 56 34 12 01 02 2003)))
(define tai-date (date->time-tai (make-date 0 56 34 12 01 02 2003)))

; Tests ------------------------------------------

(define syntax-prerender-tests
  (test-suite "syntax-prerender.ss"
    
    (test-case "xml: literals"
      (check-equal? (xml #t)           (make-raw "yes")                      "true")
      (check-equal? (xml #f)           (make-raw "")                         "false")
      (check-equal? (xml 12345)        (make-raw "12345")                    "number")
      (check-equal? (xml "blah &\"<>") (make-raw "blah &amp;&quot;&lt;&gt;") "string")
      (check-equal? (xml 'blah\&\"<>)  (make-raw "blah&amp;&quot;&lt;&gt;")  "symbol")
      (check-equal? (xml #"blah&\"<>") (make-raw "blah&amp;&quot;&lt;&gt;")  "bytes")
      (check-equal? (xml ,utc-date)    (make-atom utc-date)                  "time-utc")
      (check-equal? (xml ,tai-date)    (make-atom tai-date)                  "time-tai"))
    
    (test-case "xml: raw"
      (check-equal? (xml (!raw "&\"<>"))   (make-raw "&\"<>")                  "string")
      (check-equal? (xml (!raw 'dave))     (make-raw "dave")                   "symbol")
      (check-equal? (xml (!raw ,test-url)) (make-raw "http://www.example.com") "unquote"))
    
    (test-case "xml: comment"
      (check-equal? (xml (!comment "&\"<>"))   (make-raw "<!--&\"<>-->")               "string")
      (check-equal? (xml (!comment 'dave))     (make-raw "<!--dave-->")                "symbol")
      (check-equal? (xml (!comment ,test-url)) (make-comment "http://www.example.com") "unquote"))
    
    (test-case "xml: cdata"
      (check-equal? (xml (!cdata "&\"<>"))   (make-raw "<![CDATA[&\"<>]]>")        "string")
      (check-equal? (xml (!cdata 'dave))     (make-raw "<![CDATA[dave]]>")         "symbol")
      (check-equal? (xml (!cdata ,test-url)) (make-cdata "http://www.example.com") "unquote"))
    
    (test-case "xml: pi"
      (check-equal? (xml (!pi "&\"<>"))   (make-raw "<?&\"<>?>")             "string")
      (check-equal? (xml (!pi 'dave))     (make-raw "<?dave?>")              "symbol")
      (check-equal? (xml (!pi ,test-url)) (make-pi "http://www.example.com") "unquote"))
    
    (test-case "xml: entities"
      (check-equal? (xml (& nbsp)) (make-raw "&nbsp;")  "symbol")
      (check-equal? (xml (& 1234)) (make-raw "&#1234;") "integer"))
    
    (test-case "xml: elements"
      (check-equal? (xml (br))                      (make-raw "<br />")                  "empty")
      (check-equal? (xml (h1 "Dave"))               (make-raw "<h1>Dave</h1>")           "children")
      (check-equal? (xml (hr (@ [class "narrow"]))) (make-raw "<hr class=\"narrow\" />") "attributes")
      (check-equal? (xml (span (@ [title "&\"<>"]) "stuff")) 
                    (make-raw "<span title=\"&amp;&quot;&lt;&gt;\">stuff</span>")
                    "attributes and children")
      (check-equal? (xml (a (@ ,(make-attribute 'href test-url) [class ,text]) ,text)) 
                    (make-block (list (make-raw "<a")
                                      (make-raw (format " href=\"~a\"" test-url))
                                      (make-raw " class=\"")
                                      (make-atom text)
                                      (make-raw "\">")
                                      (make-atom text)
                                      (make-raw "</a>")))
                    "unquote in attributes and children")
      (check-equal? (xml (a (@ ,@(list (make-attribute 'href test-url) 
                                       (make-attribute 'class "blue")))
                            ,@(list text text)))
                    (make-block (list (make-raw "<a")
                                      (make-block (list (make-raw (format " href=\"~a\"" test-url))
                                                        (make-raw " class=\"blue\"")))
                                      (make-raw ">")
                                      (make-block (list (make-atom text)
                                                        (make-atom text)))
                                      (make-raw "</a>")))
                    "unquote-splicing in attributes and children"))
    
    (test-case "nested elements"
      (check-equal? (xml (a (b (c))))
                    (make-raw "<a><b><c /></b></a>")
                    "single children"))
    
    (test-equal? "top level unquote"
      (xml ,"stuff")
      (make-atom "stuff"))))

; Provide statements -----------------------------

(provide syntax-prerender-tests)
