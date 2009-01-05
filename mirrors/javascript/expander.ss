#lang scheme/base

(require (for-syntax scheme/base
                     "expander-internal.ss")
         "../base.ss"
         "expander-internal.ss")

; (_ id (syntax -> syntax) (syntax -> syntax))
(define-syntax (define-javascript-syntax stx)
  (syntax-case stx ()
    [(_ (id arg ...) expansion)
     #'(define-javascript-syntax id
         (lambda (stx)
           (syntax-case stx ()
             [(id arg ...)
              (syntax/loc stx expansion)])))]
    [(_ id js-xform)
     #'(define-javascript-syntax id
         js-xform
         (lambda (stx) 
           (raise-syntax-error 
            #f "must be used as a javascript expression" stx)))]
    [(_ id js-xform expr-xform)
     (quasisyntax/loc stx
       (define-syntax id
         (make-javascript-expander
          js-xform
          #,(if (identifier? #'expr-xform)
                #'(lambda (stx)
                    (syntax-case stx (set!)
                      [(_ args (... ...)) #'(expr-xform args (... ...))]
                      [_ #'expr-xform]))
                #'expr-xform)
          (syntax-local-certifier))))]))

; syntax -> boolean
(define (javascript-expander-syntax? stx)
  (and (identifier? stx)
       (javascript-expander? (syntax-local-value stx (lambda () #f)))))

; syntax -> syntax
(define (javascript-expand stx)
  (syntax-case stx ()
    [(id arg ...)
     (let* ([expander (syntax-local-value #'id)]
            [js-xform (and expander (javascript-expander-js-xform expander))])
       (if (and expander js-xform)
           (js-xform stx)
           (raise-syntax-error #f "not a javascript expander" stx #'id)))]))

; Provide statements -----------------------------

(provide define-javascript-syntax)

(provide/contract
 [javascript-expander-syntax? (-> syntax? boolean?)]
 [javascript-expand           (-> syntax? syntax?)])
