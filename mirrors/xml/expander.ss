#lang scheme/base

(require (for-syntax scheme/base
                     "expander-internal.ss")
         "../base.ss"
         "expander-internal.ss")

; (_ id (syntax -> syntax) (syntax -> syntax))
(define-syntax (define-xml-syntax stx)
  (syntax-case stx ()
    [(_ (id arg ...) expansion)
     #'(define-xml-syntax id
         (lambda (stx)
           (syntax-case stx ()
             [(id arg ...)
              (syntax/loc stx expansion)])))]
    [(_ id xml-xform)
     #'(define-xml-syntax id
         xml-xform
         (lambda (stx) 
           (raise-syntax-error 
            #f "must be used as an xml expression" stx)))]
    [(_ id xml-xform expr-xform)
     (quasisyntax/loc stx
       (define-syntax id
         (make-xml-expander
          xml-xform
          #,(if (identifier? #'expr-xform)
                #'(lambda (stx)
                    (syntax-case stx (set!)
                      [(_ args (... ...)) #'(expr-xform args (... ...))]
                      [_ #'expr-xform]))
                #'expr-xform)
          (syntax-local-certifier))))]))

; syntax -> boolean
(define (xml-expander-syntax? stx)
  (and (identifier? stx)
       (xml-expander? (syntax-local-value stx (lambda () #f)))))

; syntax -> syntax
(define (xml-expand stx)
  (syntax-case stx ()
    [(id arg ...)
     (let* ([expander  (syntax-local-value #'id)]
            [xml-xform (and expander (xml-expander-xml-xform expander))])
       (if (and expander xml-xform)
           (xml-xform stx)
           (raise-syntax-error #f "not an xml expander" stx #'id)))]))

; Provide statements -----------------------------

(provide define-xml-syntax)

(provide/contract
 [xml-expander-syntax? (-> syntax? boolean?)]
 [xml-expand           (-> syntax? syntax?)])
