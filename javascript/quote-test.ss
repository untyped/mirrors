#lang scheme/base

(require "../test-base.ss"
         "quote.ss"
         "struct.ss")

(define quote-tests
  (test-suite "quote.ss"
    
    (test-case "quote-id"
      (check-equal? (quote-identifier (make-Identifier #f 'a)) (make-Identifier #f 'a) "id")
      (check-equal? (quote-identifier 'a) (make-Identifier #f 'a) "symbol"))
    
    (test-case "quote-expression"
      (check-equal? (quote-expression (make-Identifier #f 'a))
                    (make-VarReference #f (make-Identifier #f 'a))
                    "identifier")
      (check-equal? (quote-expression #f)     (make-BooleanLiteral #f #f)   "boolean")
      (check-equal? (quote-expression 1)      (make-NumericLiteral #f 1)    "number")
      (check-equal? (quote-expression "abc")  (make-StringLiteral #f "abc") "string")
      (check-equal? (quote-expression 'abc)   (make-StringLiteral #f "abc") "symbol")
      (check-equal? (quote-expression #"abc") (make-StringLiteral #f "abc") "bytes")
      (check-equal? (quote-expression (string->url "http://www.example.com"))
                    (make-StringLiteral #f "http://www.example.com")
                    "url"))
    
    (test-case "quote-statement"
      (check-equal? (quote-statement (make-Identifier #f 'a))
                    (make-ExpressionStatement #f (make-VarReference #f (make-Identifier #f 'a)))
                    "identifier")
      (check-equal? (quote-statement #f)     (make-ExpressionStatement #f (make-BooleanLiteral #f #f))   "boolean")
      (check-equal? (quote-statement 1)      (make-ExpressionStatement #f (make-NumericLiteral #f 1))    "number")
      (check-equal? (quote-statement "abc")  (make-ExpressionStatement #f (make-StringLiteral #f "abc")) "string")
      (check-equal? (quote-statement 'abc)   (make-ExpressionStatement #f (make-StringLiteral #f "abc")) "symbol")
      (check-equal? (quote-statement #"abc") (make-ExpressionStatement #f (make-StringLiteral #f "abc")) "bytes")
      (check-equal? (quote-statement (string->url "http://www.example.com")) 
                    (make-ExpressionStatement #f (make-StringLiteral #f "http://www.example.com"))
                    "url"))
    
    (test-case "wrap-block: no statements"
      (check-equal? (wrap-block null)
                    (make-BlockStatement #f (list (make-EmptyStatement #f)))))
    
    (test-case "wrap-block: one non-block statement"
      (check-equal? (wrap-block (list 'x)) 
                    (make-ExpressionStatement #f (make-StringLiteral #f "x"))))
    
    (test-case "wrap-block: one block statement"
      (check-equal? (wrap-block (list (make-BlockStatement #f (list)))) 
                    (make-BlockStatement #f (list))))
    
    (test-case "wrap-block: multiple statements"
      (check-equal? (wrap-block (list 'x 'y)) 
                    (make-BlockStatement #f (list (make-ExpressionStatement #f (make-StringLiteral #f "x"))
                                                  (make-ExpressionStatement #f (make-StringLiteral #f "y"))))))
    
    ))

; Provide statements -----------------------------

(provide quote-tests)
