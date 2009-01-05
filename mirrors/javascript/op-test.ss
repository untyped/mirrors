#lang scheme/base

(require (file "../test-base.ss")
         (file "op.ss"))

(define op-tests
  (test-suite "op.ss"
    
    (test-case "prefix-operator?"
      (check-true (prefix-operator? '!) "!"))
    
    (test-case "postfix-operator?"
      (check-true (postfix-operator? '++) "++")
      (check-false (postfix-operator? '!) "!"))
    
    (test-case "infix-operator?"
      (check-true (infix-operator? '==) "==")
      (check-false (infix-operator? '=) "="))
    
    (test-case "assignment-operator?"
      (check-true (assignment-operator? '=) "=")
      (check-false (assignment-operator? '==) "=="))
    
    ))

; Provide statements -----------------------------

(provide op-tests)