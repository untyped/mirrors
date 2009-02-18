#lang scheme/base

(require (for-syntax scheme/base
                     scheme/pretty
                     "syntax-expand.ss"
                     "syntax-prerender.ss")
         "../base.ss"
         "struct.ss"
         "render-internal.ss")

(define-syntax (xml stx)
  (syntax-case stx ()
    [(_ arg ...) #`(xml-quote #,(prerender-block #'(arg ...)))]))

(define-syntax (xml-attrs stx)
  (syntax-case stx ()
    [(_ [name val] ...) #`(begin #,(expand-attributes #'([name val] ...)))]))

(define-syntax (xml* stx)
  (syntax-case stx ()
    [(_ arg ...) #`(xml-quote #,(expand-block #'(arg ...)))]))

(define-syntax (xml-attrs* stx)
  (syntax-case stx ()
    [(_ [name val] ...) #`(begin #,(expand-attributes #'([name val] ...)))]))

; (_ boolean xml)
(define-syntax (opt-xml stx)
  (syntax-case stx ()
    [(opt-xml test expr ...)
     #'(if test
           (xml expr ...)
           (xml))]))

; (_ id)
; (_ expr id)
; (_ expr id expr)
(define-syntax (opt-xml-attr stx)
  (syntax-case stx ()
    [(opt-xml-attr id)
     #'(opt-xml-attr id id id)]
    [(opt-xml-attr test id)
     #'(opt-xml-attr test id id)]
    [(opt-xml-attr test id val)
     (begin (unless (identifier? #'id)
              (raise-syntax-error 'mirrors/xml "invalid XML attribute name" stx #'id))
            #'(if test
                  (xml-attrs [id ,val])
                  (xml-attrs)))]))

; Provide statements -----------------------------

(provide xml
         xml-attrs
         xml*
         xml-attrs*
         opt-xml
         opt-xml-attr)
