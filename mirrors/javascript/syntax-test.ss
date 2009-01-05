#lang scheme/base

(require (for-syntax scheme/base
                     (file "syntax-private.ss"))
         (file "../test-base.ss")
         (file "render.ss")
         (file "struct.ss")
         (file "syntax.ss"))

; Helpers ----------------------------------------

(define-syntax (test-js stx)
  (syntax-case stx ()
    [(_ message actual expected)
     (with-handlers ([exn? (lambda (exn)
                             #`(test-case message
                                 (fail #,(format "Could not compile javascript: ~a" (exn-message exn)))))])
       (with-syntax ([expanded (expand-javascript #'actual)])
         #`(test-case message
             (with-check-info (['compiled expanded])
               (check-equal? (javascript->string expanded)
                             expected)))))]))

; Tests ------------------------------------------

(define syntax-tests
  (test-suite "syntax.ss"
    
    (test-js "decl: function"
      (function sum3 (a b c) (return (+ a b c)))
      "function sum3(a, b, c) { return a + b + c; }")
    
    (test-js "decl: var"
      (var [x 1] [y (+ 2 3)])
      "var x = 1, y = 2 + 3;")
    
    (test-js "decl: var unquote"
      (var [,(make-Identifier #f 'x) 1] [y ,(+ 2 3)])
      "var x = 1, y = 5;")
    
    (test-js "stmt: empty begin" (!begin) "")
    
    (test-js "stmt: begin"
      (!begin (+ 1 2 3)
              (!begin (var [x (+ 2 3 4)]))
              (+ 3 4 5))
      "1 + 2 + 3; var x = 2 + 3 + 4; 3 + 4 + 5;")
    
    (test-js "stmt: empty block" (!block) "{}")
    
    (test-js "stmt: block"
      (!block (+ 1 2 3)
              (!block (var [x (+ 2 3 4)]))
              (+ 3 4 5))
      "{ 1 + 2 + 3; { var x = 2 + 3 + 4; } 3 + 4 + 5; }")
    
    (test-js "stmt: if version 1"
      (if x (return y))
      "if (x) return y; ")
    
    (test-js "stmt: if version 2"
      (if x (return y) (return z))
      "if (x) return y; else return z;")
    
    (test-js "stmt: for-in version 1" 
      (for-in (x (!array 1 2 3))
        (alert x))
      "for (x in [ 1, 2, 3 ]) alert(x);")
    
    (test-js "stmt: for-in version 2"
      (for-in ((var x) (!array 1 2 3))
        (alert x))
      "for (var x in [ 1, 2, 3 ]) alert(x);")
    
    (test-js "stmt: for-in version 3"
      (for-in ([key val] (!object [a 1] [b 2] [c 3]))
        (alert (+ key ": " val)))
      "for (key, val in { a: 1, b: 2, c: 3 }) alert(key + \": \" + val);")
    
    (test-js "stmt: for-in version 4"
      (for-in ((var key val) (!object [a 1] [b 2] [c 3]))
        (alert (+ key ": " val)))
      "for (var key, val in { a: 1, b: 2, c: 3 }) alert(key + \": \" + val);")
    
    (test-js "stmt: try version 1"
      (try (+ 1 2) (+ 2 3) (catch e (+ 3 4)) (finally (+ 4 5)))
      "try { 1 + 2; 2 + 3; } catch (e) { 3 + 4; } finally { 4 + 5; }")
    
    (test-js "stmt: try version 2"
      (try (+ 1 2) (catch e (+ 3 4)) (finally (+ 4 5)))
      "try { 1 + 2; } catch (e) { 3 + 4; } finally { 4 + 5; }")
    
    (test-js "stmt: unquote"
      (!begin (while (< x 10) (post++ x))
              ,(js (while (> x 5) (post-- x))
                   (while (< x 15) (pre++ x))))
      "while (x < 10) x++; while (x > 5) x--; while (x < 15) ++x;")
    
    (test-js "stmt: unquote-splicing"
      (!begin (while (< x 10) (post++ x))
              ,@(list (js (while (> x 5) (post-- x)))
                      (js (while (< x 15) (pre++ x)))))
      "while (x < 10) x++; while (x > 5) x--; while (x < 15) ++x;")
    
    (test-js "expr: array"    (!array x "y" 123)            "[ x, \"y\", 123 ];")
    (test-js "expr: object"   (!object [x 1] ["y" 2] [3 4]) "{ x: 1, \"y\": 2, 3: 4 };")
    (test-js "expr: dot"      (!dot x y z)                  "x.y.z;")
    (test-js "expr: index"    (!index (getStuff 1) (+ 2 3)) "getStuff(1)[2 + 3];")
    (test-js "expr: infix"    (+ 1 2 3)                     "1 + 2 + 3;")
    (test-js "expr: prefix"   (pre++ x)                     "++x;")
    (test-js "expr: postfix"  (post++ x)                    "x++;")
    (test-js "expr: call"     (x y z)                       "x(y, z);")
    
    (test-js "expr: conditional"
      (? (== a b) c d)
      "a == b ? c : d;")
    
    (test-js "expr: anonymous function wrapper"
      ((function () (alert "Dave")))
      "(function() { alert(\"Dave\"); })();")
    
    (test-js "expr: unquote"
      (return (+ 1 ,(js (/ 4 2)) 3))
      "return 1 + 4 / 2 + 3;")
    
    (test-js "expr: function with local variable declarations"
      ((function () (var [x 1]) (return x)))
      "(function() { var x = 1; return x; })();")
    
    ))

; Provide statements -----------------------------

(provide syntax-tests)