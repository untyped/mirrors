#lang scheme/base

(require (for-syntax scheme/base
                     (file "op-util-internal.ss")
                     (file "op.ss"))
         scheme/contract
         (file "struct.ss"))

; Syntax -----------------------------------------

(define-syntax (define/provide stx)
  (syntax-case stx ()
    [(_ id contract expr)
     #'(begin (define id expr)
              (provide/contract [id contract]))]
    [(_ [id global-id] contract expr)
     #'(begin (define id expr)
              (provide/contract [rename id global-id contract]))]))

(define-syntax (define-prefix-expr-constructors stx)
  (make-expr-constructors stx scheme-prefix-operators prefix-operators #'make-PrefixExpression #'PrefixExpression? 1))

(define-syntax (define-postfix-expr-constructors stx)
  (make-expr-constructors stx scheme-postfix-operators postfix-operators #'make-PostfixExpression #'PostfixExpression? 1))

(define-syntax (define-infix-expr-constructors stx)
  (make-expr-constructors stx infix-operators infix-operators #'make-InfixExpression #'InfixExpression? #f))

(define-syntax (define-assignment-expr-constructors stx)
  (make-expr-constructors stx assignment-operators assignment-operators #'make-AssignmentExpression #'AssignmentExpression? 2))

; Provide statements -----------------------------

(provide define/provide
         define-prefix-expr-constructors
         define-postfix-expr-constructors
         define-infix-expr-constructors
         define-assignment-expr-constructors)