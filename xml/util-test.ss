#lang scheme/base

(require (for-syntax scheme/base)
         "../javascript/javascript.ss"
         "../test-base.ss"
         "render.ss"
         "struct.ss"
         "syntax.ss"
         "util.ss")

; Tests ------------------------------------------

(define/provide-test-suite util-tests
  
  (test-equal? "alist->attributes"
    (alist->attributes `((title . "title")
                         (rowspan . 2)
                         (onclick . ,(js (alert "clicked")))))
    (list (make-attribute 'title "title")
          (make-attribute 'rowspan 2)
          (make-attribute 'onclick (js (alert "clicked")))))
  
  (test-case "opt-xml"
    (check-equal? (xml->string (xml "[" ,(opt-xml (even? 2) (span "stuff")) "]")) "[<span>stuff</span>]")
    (check-equal? (xml->string (xml "[" ,(opt-xml (even? 1) (span "stuff")) "]")) "[]")
    (check-equal? (xml->string (xml "[" ,(opt-xml (even? 2) (span "a") (span "b")) "]")) "[<span>a</span><span>b</span>]")
    (check-equal? (xml->string (xml "[" ,(opt-xml (even? 1) (span "a") (span "b")) "]")) "[]")
    (check-equal? (xml->string (xml* "[" ,(opt-xml (even? 2) (span "stuff")) "]")) "[<span>stuff</span>]")
    (check-equal? (xml->string (xml* "[" ,(opt-xml (even? 1) (span "stuff")) "]")) "[]"))
  
  (test-case "opt-xml-attr"
    (let ([class   'theclass]
          [noclass #f])
      (check-equal? (xml->string (xml (br (@ ,(opt-xml-attr class)))))   "<br class=\"theclass\" />")
      (check-equal? (xml->string (xml (br (@ ,(opt-xml-attr noclass))))) "<br />")
      (check-equal? (xml->string (xml (br (@ ,(opt-xml-attr class noclass))))) "<br noclass=\"\" />")
      (check-equal? (xml->string (xml (br (@ ,(opt-xml-attr noclass class))))) "<br />")
      (check-equal? (xml->string (xml (br (@ ,(opt-xml-attr class noclass 123))))) "<br noclass=\"123\" />")
      (check-equal? (xml->string (xml (br (@ ,(opt-xml-attr noclass class 123))))) "<br />"))))
