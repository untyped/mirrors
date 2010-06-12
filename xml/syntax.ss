#lang scheme

(require "../base.ss")

(require (for-syntax "../base.ss"
                     "syntax-expand.ss"
                     "syntax-prerender.ss")
         "struct.ss"
         "render-internal.ss")

; Lowercase variants -----------------------------

(define-syntax (xml stx)
  (syntax-case stx ()
    [(_ arg ...)
     #`(xml-quote #,(parameterize ([quote-case-restriction 'lower])
                      (prerender-block #'(arg ...))))]))

(define-syntax (xml-attrs stx)
  (syntax-case stx ()
    [(_ [name val] ...)
     #`(begin #,(parameterize ([quote-case-restriction 'lower])
                  (expand-attributes #'([name val] ...))))]))

(define-syntax (xml* stx)
  (syntax-case stx ()
    [(_ arg ...)
     #`(xml-quote #,(parameterize ([quote-case-restriction 'lower])
                      (expand-block #'(arg ...))))]))

(define-syntax (xml-attrs* stx)
  (syntax-case stx ()
    [(_ [name val] ...)
     #`(begin #,(parameterize ([quote-case-restriction 'lower])
                  (expand-attributes #'([name val] ...))))]))

; (_ boolean xml)
(define-syntax (opt-xml stx)
  (syntax-case stx ()
    [(_ test expr ...)
     #'(if test
           (xml expr ...)
           (xml))]))

; (_ id)
; (_ expr id)
; (_ expr id expr)
(define-syntax (opt-xml-attr stx)
  (syntax-case stx ()
    [(_ id)      #'(opt-xml-attr id id id)]
    [(_ test id) #'(opt-xml-attr test id id)]
    [(_ test id val)
     (begin (unless (identifier? #'id)
              (raise-syntax-error 'mirrors/xml "invalid XML attribute name" stx #'id))
            #'(if test
                  (xml-attrs [id ,val])
                  (xml-attrs)))]))

; (_ string xml ...)
(define-syntax-rule (debug-xml msg expr ...)
  (let ([ans (xml expr ...)])
    (printf "----------~a:~n~a~n----------~n" msg (xml->string ans))
    ans))

; Uppercase variants -----------------------------

(define-syntax (XML stx)
  (syntax-case stx ()
    [(_ arg ...)
     #`(xml-quote #,(parameterize ([quote-case-restriction 'upper])
                      (prerender-block #'(arg ...))))]))

(define-syntax (XML-ATTRS stx)
  (syntax-case stx ()
    [(_ [name val] ...)
     #`(begin #,(parameterize ([quote-case-restriction 'upper])
                  (expand-attributes #'([name val] ...))))]))

(define-syntax (XML* stx)
  (syntax-case stx ()
    [(_ arg ...)
     #`(xml-quote #,(parameterize ([quote-case-restriction 'upper])
                      (expand-block #'(arg ...))))]))

(define-syntax (XML-ATTRS* stx)
  (syntax-case stx ()
    [(_ [name val] ...)
     #`(begin #,(parameterize ([quote-case-restriction 'upper])
                  (expand-attributes #'([name val] ...))))]))

; (_ boolean xml)
(define-syntax (OPT-XML stx)
  (syntax-case stx ()
    [(_ test expr ...)
     #'(if test
           (XML expr ...)
           (XML))]))

; (_ id)
; (_ expr id)
; (_ expr id expr)
(define-syntax (OPT-XML-ATTR stx)
  (syntax-case stx ()
    [(_ id)          #'(OPT-XML-ATTR id id id)]
    [(_ test id)     #'(OPT-XML-ATTR test id id)]
    [(_ test id val)
     (begin (unless (identifier? #'id)
              (raise-syntax-error 'mirrors/xml "invalid XML attribute name" stx #'id))
            #'(if test
                  (XML-ATTRS [id ,val])
                  (XML-ATTRS)))]))

; (_ string xml ...)
(define-syntax-rule (DEBUG-XML msg expr ...)
  (let ([ans (XML expr ...)])
    (printf "----------~a:~n~a~n----------~n" msg (xml->string ans))
    ans))

; Provide statements -----------------------------

(provide xml xml-attrs xml* xml-attrs* opt-xml opt-xml-attr debug-xml
         XML XML-ATTRS XML* XML-ATTRS* OPT-XML OPT-XML-ATTR DEBUG-XML)
