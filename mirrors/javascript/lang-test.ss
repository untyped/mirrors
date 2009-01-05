#lang scheme/base

(require srfi/26/cut
         (file "../test-base.ss")
         (prefix-in js: (file "lang.ss"))
         (file "struct.ss"))

; Helpers ----------------------------------------

(define symbols
  '(a b c))

(define ids
  (map (cut make-Identifier #f <>)
       symbols))

(define numbers
  '(1 2 3))

(define numeric-lits 
  (map (cut make-NumericLiteral #f <>)
       numbers))

; Tests ------------------------------------------

(define lang-tests
  (test-suite "lang.ss"
    
    (test-case "js:var"
      (check-exn exn:fail:contract? js:var "no args")
      (check-equal? (apply js:var symbols)
                    (make-VariableDeclaration #f (map (cut make-VariableInitializer #f <> <>) ids '(#f #f #f)))
                    "no initializers")
      (check-equal? (apply js:var (map js:init symbols numbers))
                    (make-VariableDeclaration #f (map (cut make-VariableInitializer #f <> <>) ids numeric-lits))
                    "initializers"))
    
    (test-case "js:begin"
      (check-equal? (js:begin)
                    (make-BeginStatement #f (list))
                    "no args")
      (check-equal? (js:begin 1 2 3)
                    (make-BeginStatement #f (map (cut make-ExpressionStatement #f <>) numeric-lits))
                    "multiple args"))
    
    (test-case "js:block"
      (check-equal? (js:block)
                    (make-BlockStatement #f null)
                    "no args")
      (check-equal? (js:block 1 2 3)
                    (make-BlockStatement #f (map (cut make-ExpressionStatement #f <>) numeric-lits))
                    "args"))
    
    (test-case "js:if"
      (check-equal? (apply js:if numbers)
                    (make-IfStatement #f
                                      (make-NumericLiteral #f 1) 
                                      (make-ExpressionStatement #f (make-NumericLiteral #f 2))
                                      (make-ExpressionStatement #f (make-NumericLiteral #f 3)))))
    
    (test-case "js:do"
      (check-equal? (js:do 1 #:while 2)
                    (make-DoWhileStatement #f
                                           (make-ExpressionStatement #f (make-NumericLiteral #f 1))
                                           (make-NumericLiteral #f 2))))
    
    (test-case "js:while"
      (check-equal? (js:while 1 2)
                    (make-WhileStatement #f
                                         (make-NumericLiteral #f 1)
                                         (make-ExpressionStatement #f (make-NumericLiteral #f 2)))))
    #|
    (test-case "js:for" (fail "Not implemented."))
    (test-case "js:for-in" (fail "Not implemented."))
    (test-case "js:break" (fail "Not implemented."))
    (test-case "js:continue" (fail "Not implemented."))
    (test-case "js:return" (fail "Not implemented."))
    (test-case "js:with" (fail "Not implemented."))
    (test-case "js:switch" (fail "Not implemented."))
    (test-case "js:case" (fail "Not implemented."))
    (test-case "js:default" (fail "Not implemented."))
    (test-case "js:label" (fail "Not implemented."))
    (test-case "js:throw" (fail "Not implemented."))
    (test-case "js:try" (fail "Not implemented."))
    (test-case "js:catch" (fail "Not implemented."))
    (test-case "js:finally" (fail "Not implemented."))
    (test-case "js:array" (fail "Not implemented."))
    (test-case "js:object" (fail "Not implemented."))
    (test-case "js:field" (fail "Not implemented."))
    (test-case "js:index" (fail "Not implemented."))
    (test-case "js:dot" (fail "Not implemented."))
    (test-case "js:new" (fail "Not implemented."))
    (test-case "js:pre++" (fail "Not implemented."))
    (test-case "js:post++" (fail "Not implemented."))
    (test-case "js:+" (fail "Not implemented."))
    (test-case "js:==" (fail "Not implemented."))
    (test-case "js:function" (fail "Not implemented."))
    (test-case "js:all" (fail "Not implemented."))
    (test-case "js:call" (fail "Not implemented."))
    (test-case "js:id" (fail "Not implemented."))
    |#
    ))

; Provide statements -----------------------------

(provide lang-tests)