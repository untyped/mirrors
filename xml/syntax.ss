#lang scheme/base

(require (for-syntax scheme/base
                     scheme/pretty
                     (file "syntax-expand.ss")
                     (file "syntax-prerender.ss"))
         scheme/pretty
         (file "struct.ss")
         (file "render-private.ss"))

(define-syntax (xml stx)
  (syntax-case stx ()
    [(_ arg ...) #`(begin #,(prerender-block #'(arg ...)))]))

(define-syntax (xml-attrs stx)
  (syntax-case stx ()
    [(_ [name val] ...) #`(begin #,(expand-attributes #'([name val] ...)))]))

(define-syntax (xml* stx)
  (syntax-case stx ()
    [(_ arg ...) #`(begin #,(expand-block #'(arg ...)))]))

; Provide statements -----------------------------

(provide xml
         xml-attrs
         xml*)