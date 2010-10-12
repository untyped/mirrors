#lang scheme/base

(require "../test-base.ss")

(require (for-syntax scheme/base
                     "syntax-prerender.ss"
                     "syntax-expand.ss")
         "../javascript/javascript.ss"
         "expander.ss"
         "render.ss"
         "struct.ss"
         "syntax.ss")

; Helpers ----------------------------------------

(define url1     "http://www.example.com")
(define url2     (string->url "http://www.example.com?a=b&c=d"))
(define text     "Text")
(define sym      'symbol)

; GMT:
(define utc-winter-date (date->time-utc (make-date 0 56 34 12 01 02 2003)))
(define tai-winter-date (date->time-tai (make-date 0 56 34 12 01 02 2003)))

; BST:
(define utc-summer-date (date->time-utc (make-date 0 56 34 12 01 07 2003)))
(define tai-summer-date (date->time-tai (make-date 0 56 34 12 01 07 2003)))

(define-xml-syntax (!wrap expr1 expr2)
  (xml expr1 expr2 expr1))

(define-syntax (check-xml stx)
  (syntax-case stx ()
    [(check-xml expr expected)
     #'(check-xml expr expected "no message")]
    [(check-xml expr expected msg)
     (with-syntax ([prerendered-expr #'(xml expr)]
                   [expanded-expr    #'(xml* expr)])
       #`(begin (with-check-info (['prerendered '#,(syntax->datum #'prerendered-expr)])
                  (check-equal? (xml->string prerendered-expr) expected (format "xml: ~a" msg)))
                (with-check-info (['expanded '#,(syntax->datum #'expanded-expr)])
                  (check-equal? (xml->string expanded-expr) expected (format "xml*: ~a" msg)))))]))

; Tests ------------------------------------------

(define/provide-test-suite render-tests
  
  (test-case "literals"
    (check-xml #t               "yes"                      "true")
    (check-xml #f               ""                         "false")
    (check-xml 12345            "12345"                    "number")
    (check-xml "blah &\"<>"     "blah &amp;&quot;&lt;&gt;" "string")
    (check-xml 'blah\&\"<>      "blah&amp;&quot;&lt;&gt;"  "symbol")
    (check-xml #"blah &\"<>"    "blah &amp;&quot;&lt;&gt;" "bytes")
    ; Times are rendered in the correct immediate time zone:
    (check-xml ,utc-winter-date "2003-02-01 12:34:56" "time-utc (GMT)")
    (check-xml ,tai-winter-date "2003-02-01 12:34:56" "time-tai (GMT)")
    (check-xml ,utc-summer-date "2003-07-01 12:34:56" "time-utc (BST)")
    (check-xml ,tai-summer-date "2003-07-01 12:34:56" "time-tai (BST)")
    (parameterize ([current-tz "PST8PDT"])
      (check-xml ,utc-winter-date "2003-02-01 04:34:56" "time-utc (PST)")
      (check-xml ,tai-winter-date "2003-02-01 04:34:56" "time-tai (PST)")
      (check-xml ,utc-summer-date "2003-07-01 04:34:56" "time-utc (PDT)")
      (check-xml ,tai-summer-date "2003-07-01 04:34:56" "time-tai (PDT)")))
  
  (test-case "raw"
    (check-xml (!raw "&\"<>") "&\"<>"                          "string")
    (check-xml (!raw 'dave)   "dave"                           "symbol")
    (check-xml (!raw ,url2)   "http://www.example.com?a=b&c=d" "unquote")
    (check-xml (!raw "a" 123) "a123"                           "multiple arguments"))
  
  (test-case "comment"
    (check-xml (!comment "&\"<>") "<!--&\"<>-->"                  "string")
    (check-xml (!comment 'dave)   "<!--dave-->"                   "symbol")
    (check-xml (!comment ,url1)   "<!--http://www.example.com-->" "unquote")
    (check-xml (!comment "a" 123) "<!--a123-->"                   "multiple arguments"))
  
  (test-case "cdata"
    (check-xml (!cdata "&\"<>") "<![CDATA[&\"<>]]>"                  "string")
    (check-xml (!cdata 'dave)   "<![CDATA[dave]]>"                   "symbol")
    (check-xml (!cdata ,url1)   "<![CDATA[http://www.example.com]]>" "unquote")
    (check-xml (!cdata "a" 123) "<![CDATA[a123]]>"                   "multiple arguments")
    (check-xml (script (!cdata "\n" ,(js (alert "Clicked")) "\n// "))
               "<script><![CDATA[\nalert(\"Clicked\")\n// ]]></script>"
               "unquote to js"))
  
  (test-case "pi"
    (check-xml (!pi "&\"<>") "<?&\"<>?>"                  "string")
    (check-xml (!pi 'dave)   "<?dave?>"                   "symbol")
    (check-xml (!pi ,url1)   "<?http://www.example.com?>" "unquote")
    (check-xml (!pi "a" 123) "<?a123?>"                   "multiple arguments"))
  
  (test-case "entities"
    (check-xml (& nbsp) "&nbsp;"   "symbol")
    (check-xml (& 1234) "&#1234;"  "integer")
    (check-xml (& ,sym) "&symbol;" "unquote"))
  
  (test-case "elements"
    (check-xml (br)
               "<br />"
               "empty")
    (check-xml (h1 "Dave")
               "<h1>Dave</h1>"
               "children")
    (check-xml (hr (@ [class "narrow"]))
               "<hr class=\"narrow\" />" 
               "attributes")
    (check-xml (script (@ [src "file.js"]))
               "<script src=\"file.js\"></script>"
               "attributes, no children, preserve singletons")
    (check-xml (span (@ [title "&\"<>"]) "stuff")
               "<span title=\"&amp;&quot;&lt;&gt;\">stuff</span>"
               "attributes and children")
    (check-xml (a (@ ,(make-attribute 'href url1)) ,text)
               "<a href=\"http://www.example.com\">Text</a>"
               "unquote in attributes and children")
    (check-xml (a (@ ,@(list (make-attribute 'href url1) (make-attribute 'class "blue"))) ,@(list text text))
               "<a href=\"http://www.example.com\" class=\"blue\">TextText</a>"
               "unquote-splicing in attributes and children")
    (check-xml (span (@ ,@(xml-attrs [title "title"] ,@null)) "text")
               "<span title=\"title\">text</span>"
               "unquote-splicing with null in attributes")
    (check-xml (script ,(js (alert "Clicked 1")))
               "<script>alert(&quot;Clicked 1&quot;)</script>"
               "unquote to js"))
  
  (test-case "expander"
    (check-xml (!wrap "a" "b")
               "aba")
    (check-xml (!wrap "a" (!wrap "b" "c"))
               "abcba")
    (check-xml (!wrap (tag1) (!wrap (tag2) (tag3)))
               "<tag1 /><tag2 /><tag3 /><tag2 /><tag1 />"))
  
  (test-case "javascript attribute"
    (check-xml (a (@ [onclick ,(js (alert "Clicked 1") (alert "Clicked 2") (alert "Clicked 3"))]) "Click here")
               "<a onclick=\"alert(&quot;Clicked 1&quot;); alert(&quot;Clicked 2&quot;); alert(&quot;Clicked 3&quot;);\">Click here</a>"))
  
  (test-equal? "xml-attrs and javascript attribute values"
    (xml->string (xml (a (@ ,@(xml-attrs [onclick ,(js (alert 10))])) "blah")))
    "<a onclick=\"alert(10)\">blah</a>")
  
  (test-equal? "xml-attrs and javascript attribute values"
    (xml->string (xml (a (@ ,(xml-attrs [onclick ,(js (alert 10))])) "blah")))
    "<a onclick=\"alert(10)\">blah</a>"))
