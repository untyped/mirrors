#lang scheme/base

(require (for-syntax scheme/base
                     "../test-base.ss"
                     "syntax-internal.ss")
         (prefix-in scheme: scheme/pretty)
         srfi/13
         "../test-base.ss"
         "expander.ss"
         "render.ss"
         "struct.ss"
         "syntax.ss")

; Helpers ----------------------------------------

(define-syntax (test-js stx)
  (parameterize ([quote-case-restriction 'lower])
    (syntax-case stx ()
      [(_ message actual expected)
       (with-handlers ([exn? (lambda (exn)
                               #`(test-case message
                                   (fail #,(format "Could not compile javascript: ~a"
                                                   (exn-message exn)))))])
         (with-syntax ([expanded (expand-javascript #'actual)])
           #`(test-case message
               (with-check-info (['compiled expanded])
                 (check-equal? (string-trim-both (javascript->string expanded))
                               expected)))))])))

(define-javascript-syntax (!var-debug [id expr] ...)
  (js (!begin (var [id expr])
              (!dot console (log (+ 'id id))))
      ...))

(define-javascript-syntax (!fast-for (id init test incr) stmt ...)
  (js (var [id init])
      (while (! test)
        stmt ...
        incr)))

(define-javascript-syntax (!max a b)
  (js (? (> a b) a b)))

; Tests ------------------------------------------

(define syntax-tests
  (test-suite "syntax.ss"
    
    #:before (cut javascript-rendering-mode 'packed)
    
    (test-js "expander: decl"
      (!var-debug [a 1] [b 2])
      "var a = 1; console.log(\"a\" + a); var b = 2; console.log(\"b\" + b);")
    
    (test-js "expander: stmt"
      (!fast-for (i 0 (< i 10) (= i (+ i 1))) (alert i))
      "var i = 0; while (!(i < 10)) { alert(i); i = i + 1; }")
    
    (test-js "expander: expr"
      (!max (!max a b) c)
      "(a > b ? a : b) > c ? a > b ? a : b : c;")
    
    (test-js "decl: function"
      (function sum3 (a b c) (return (+ a b c)))
      "function sum3(a, b, c) { return a + b + c; }")
    
    (test-js "decl: anonymous function"
      (function (a b c) (return (+ a b c)))
      "function(a, b, c) { return a + b + c; }")
    
    (test-js "decl: anonymous function as an expression argument"
      (+ ((function (a b c) (return (+ a b c)))) 1)
      "(function(a, b, c) { return a + b + c; })() + 1;")
    
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
    
    (test-js "stmt: begin containing nested function declarations"
      (!begin (function a () (return))
              (function b () (return)))
      "function a() { return; } function b() { return; }")


    (test-js "stmt: empty block" (!block) "{}")
    
    (test-js "stmt: block"
      (!block (+ 1 2 3)
              (!block (var [x (+ 2 3 4)]))
              (+ 3 4 5))
      "{ 1 + 2 + 3; { var x = 2 + 3 + 4; } 3 + 4 + 5; }")
    
    (test-js "stmt: raw (statement position)"
      (!block (!raw "["))
      "{ [; }")
    
    (test-js "stmt: raw (expression position)"
      (+ 1 (!raw "[") 2)
      "1 + ([) + 2;")
    
    (test-js "stmt: if version 1"
      (if x (return y))
      "if (x) return y;")
    
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
    
    (test-exn "stmt: for-in version 4: for-in can only have one iterator variable"
      exn:fail:contract?
      (lambda ()
        (js (for-in ((var key val) (!object [a 1] [b 2] [c 3]))
              (alert (+ key ": " val))))))
    
    (test-js "stmt: throw"
      (throw "x")
      "throw \"x\";")
    
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
    (test-js "expr: new"      (new Array 1 2 3)             "new Array(1, 2, 3);")
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
    
    (test-js "expr: quote"
      (alert 'a-symbol)
      "alert(\"a-symbol\");")
    
    (test-js "expr: unquote"
      (return (+ 1 ,(js (/ 4 2)) 3))
      "return 1 + 4 / 2 + 3;")
    
    (test-js "expr: function with local variable declarations"
      ((function () (var [x 1]) (return x)))
      "(function() { var x = 1; return x; })();")
    
    (test-js "!dot: no inline procedure calls"
      ((!dot a b) c)
      "a.b(c);")
    
    (test-js "!dot: inline procedure calls only"
      (!dot (a b c) (d e f) (g h i))
      "a(b, c).d(e, f).g(h, i);")
    
    (test-js "!dot: inline procedure calls mixed with normal identifiers"
      (!dot (a b c) d e f (g h i))
      "a(b, c).d.e.f.g(h, i);")
    
    (test-js "!dot: inline procedure calls mixed with normal identifiers"
      (!dot (+ "a" "b") length)
      "(\"a\" + \"b\").length;")
    
    (test-js "!dot: first argument may be a true expression"
      (!dot (+ "a" "b") length)
      "(\"a\" + \"b\").length;")
    
    (test-js "!dot and !index"
      (!dot (!index a 1) (!index b 2) c)
      "a[1].b[2].c;")
    
    (test-js "!dot and function"
      (!dot (function () (return 1)) (b c))
      "(function() { return 1; }).b(c);")
    
    (test-js "!dot and this"
      (!dot this (doStuff))
      "this.doStuff();")
    
    (test-js "expr: !regexp: no arguments"
      (!regexp "abc")
      "/abc/;")
    
    (test-js "expr: !regexp: global"
      (!regexp "abc" #:global? #t)
      "/abc/g;")
    
    (test-js "expr: !regexp: case insensitive"
      (!regexp "abc" #:ci? #t)
      "/abc/i;")
    
    (test-js "expr: !regexp: global and case insensitive"
      (!regexp "abc" #:global? #t #:ci? #t)
      "/abc/gi;")
    
    (test-js "really long one-line program"
      (function () 
        ,@(for/list ([x (in-range 0 100)])
            (js (alert x))))
      (format "function() { ~a}" 
              (for/fold ([str ""])
                        ([x (in-range 0 100)])
                        (string-append str "alert(x); "))))
    
    (test-case "opt-js"
      (check-equal? (opt-js #t (alert "Hello world!")) (js (alert "Hello world!")))
      (check-equal? (opt-js #f (alert "Hello world!")) (js)))))

; Provide statements -----------------------------

(provide syntax-tests)
