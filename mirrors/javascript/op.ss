#lang scheme/base

(require scheme/contract
         (only-in (planet dherman/javascript:7/ast)
                  prefix-operators
                  postfix-operators
                  infix-operators
                  assignment-operators
                  prefix-operator?
                  postfix-operator?
                  infix-operator?
                  assignment-operator?))

; Operator symbols -------------------------------

; (listof symbol)
; These are used when scaffolding procedures with names based on the relevant operators,
; to make sure there are no conflicting procedure names.
(define scheme-prefix-operators
  (map (lambda (op)
         (case op
           [(++) 'pre++]
           [(--) 'pre--]
           [(+)  'pre+]
           [(-)  'pre-]
           [else op]))
       prefix-operators))

; (listof symbol)
; These are used when scaffolding procedures with names based on the relevant operators,
; to make sure there are no conflicting procedure names.
(define scheme-postfix-operators
  (map (lambda (op)
         (case op
           [(++) 'post++]
           [(--) 'post--]
           [else op]))
       postfix-operators))

; (listof symbol)
(define all-scheme-operators
  `(,@scheme-prefix-operators
    ,@scheme-postfix-operators
    ,@infix-operators
    ,@assignment-operators))

; Predicates -------------------------------------

; symbol -> boolean
(define (scheme-prefix-operator? op)
  (if (memq op scheme-prefix-operators) #t #f))

; symbol -> boolean
(define (scheme-postfix-operator? op)
  (if (memq op scheme-postfix-operators) #t #f))

; symbol -> boolean
(define (scheme-operator? op)
  (if (memq op all-scheme-operators) #t #f))

; Provide statements -----------------------------

(provide prefix-operators
         postfix-operators
         infix-operators
         assignment-operators
         prefix-operator?
         postfix-operator?
         infix-operator?
         assignment-operator?)

(provide/contract
 [scheme-prefix-operators  (listof symbol?)]
 [scheme-postfix-operators (listof symbol?)]
 [scheme-prefix-operator?  (-> any/c boolean?)]
 [scheme-postfix-operator? (-> any/c boolean?)]
 [scheme-operator?         (-> any/c boolean?)])
