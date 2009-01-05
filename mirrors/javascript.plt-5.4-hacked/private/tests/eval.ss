(module eval mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 1))
           (planet "test.ss" ("dherman" "test.plt" 1 2))
           (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 2 1))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 1))
           (planet "io.ss" ("dherman" "io.plt" 1 8))
           (lib "string.ss" "srfi" "13")
           (lib "string.ss")
           "../../eval.ss"
           "../../config.ss")

  (define test-ns (make-javascript-namespace))

  (enable-let-expressions? #t)

  (define-simple-check (check-output* expected lines)
    (reset-javascript-namespace! test-ns)
    (let ([actual (with-output-to-string (eval-javascript-string (string-join lines "\n") test-ns))])
      (andmap string=?
              expected
              (regexp-split #rx"[\r\n]+" (string-trim-both actual)))))

  (define-syntax check-output
    (syntax-rules ()
      [(_ expected lines ...)
       (check-output* expected (list lines ...))]))

  (define binding-tests
    (test-suite "binding tests"
      (test-case "top-level binding"
        (check-output '("true")
                      "var a = true;"
                      "print(a);"))
      (test-case "non-with lexical binding"
        (check-output '("true")
                      "(function(a){print(a)})(true)"))
      (test-case "non-with catch binding"
        (check-output '("true")
                      "try{throw true}catch(a){print(a)}"))
      (test-case "non-with let binding"
        (check-output '("true")
                      "let (a = true){print(a)}"))
      (test-case "non-with lexical shadowing of top-level"
        (check-output '("true")
                      "var a = false;"
                      "(function(a){print(a)})(true);"))
      (test-case "non-with lexical shadowing of lexical"
        (check-output '("true")
                      "(function(a){(function(a){print(a)})(true)})(false);"))
      (test-case "non-with lexical shadowing of catch"
        (check-output '("true")
                      "try{throw false}catch(a){(function(a){print(a)})(true)}"))
      (test-case "non-with lexical shadowing of let"
        (check-output '("true")
                      "let (a = false){(function(a){print(a)})(true)}"))
      (test-case "non-with catch shadowing of top-level"
        (check-output '("true")
                      "var a = false"
                      "try{throw true}catch(a){print(a)}"))
      (test-case "non-with catch shadowing of lexical"
        (check-output '("true")
                      "(function(a){try{throw true}catch(a){print(a)}})(false)"))
      (test-case "non-with catch shadowing of catch"
        (check-output '("true")
                      "try{throw false}catch(a){try{throw true}catch(a){print(a)}}"))
      (test-case "non-with catch shadowing of let"
        (check-output '("true")
                      "let (a = false){try{throw true}catch(a){print(a)}}"))
      #;(test-case "non-with let shadowing of top-level"
        (check-output '("true")
                      ))
      #;(test-case "non-with let shadowing of lexical"
        (check-output '("true")
                      ))
      #;(test-case "non-with let shadowing of catch"
        (check-output '("true")
                      ))
      #;(test-case "non-with let shadowing of let"
        (check-output '("true")
                      ))
      (test-case "with binding"
        (check-output '("true")
                      "var o = {a:true}"
                      "with(o){print(a)}"))
      (test-case "with shadowing of top-level"
        (check-output '("true")
                      "var o = {a:true}"
                      "var a = false"
                      "with(o){print(a)}"))
      (test-case "with shadowing of lexical"
        (check-output '("true")
                      "var o = {a:true};"
                      "(function(a){with(o){print(a)}})(false);"))
      (test-case "with shadowing of with"
        (check-output '("true")
                      "var o1 = {a:false};"
                      "var o2 = {a:true};"
                      "with(o1){with(o2){print(a)}}"))
      (test-case "with shadowing of catch"
        (check-output '("true")
                      "var o = {a:true}"
                      "try{throw false}catch(a){with(o){print(a)}}"))
      #;(test-case "with shadowing of let"
        (check-output '("true")
                      ))
      (test-case "lexical shadowing of with"
        (check-output '("true")
                      "var o = {a:false}"
                      "with(o) {(function(a){print(a)})(true)}"))
      (test-case "catch shadowing of with"
        (check-output '("true")
                      "var o = {a:false}"
                      "with(o){try{throw true}catch(a){print(a)}}"))
      #;(test-case "let shadowing of with"
        (check-output '("true")
                      ))
      (test-case "with shadowing of lexical shadowing of with"
        (check-output '("true")
                      "var o1 = {a:1}"
                      "var o2 = {a:true}"
                      "with(o1){(function(a){with(o2){print(a)}})(2)}"))
      (test-case "with shadowing of catch shadowing of with"
        (check-output '("true")
                      "var o1 = {a:1}"
                      "var o2 = {a:true}"
                      "with(o1){try{throw 2}catch(a){with(o2){print(a)}}}"))
      #;(test-case "with shadowing of let shadowing of with"
        (check-output '("true")
                      ))
      (test-case "with shadowing of catch shadowing of lexical"
        (check-output '("true")
                      "var o = {a:true};"
                      "(function(a){try{throw 1}catch(a){with(o){print(a)}}})(2)"))
      (test-case "lexical shadowing of catch shadowing of with"
        (check-output '("true")
                      "var o = {a:1};"
                      "try{throw 2}catch(a){(function(a){print(a)})(true)}"))
      #;(test-case "with shadowing of catch shadowing of let"
        (check-output '("true")
                      ))
      (test-case "lexical shadowing of with shadowing of catch"
        (check-output '("true")
                      "var o = {a:1};"
                      "try{throw 2}catch(a){with(o){(function(a){print(a)})(true)}}"))
      (test-case "catch shadowing of with shadowing of lexical"
        (check-output '("true")
                      "var o = {a:1};"
                      "(function(a){with(o){try{throw true}catch(a){print(a)}}})(2);"))
      #;(test-case "let shadowing of with shadowing of catch"
        (check-output '("true")
                      ))
      #;(test-case "let shadowing of with shadowing of lexical"
        (check-output '("true")
                      ))
      #;(test-case "lexical shadowing of with shadowing of let"
        (check-output '("true")
                      ))
      #;(test-case "catch shadowing of with shadowing of let"
        (check-output '("true")
                      ))
      (test-case "with shadowing of lexical shadowing of catch"
        (check-output '("true")
                      "var o = {a:true};"
                      "try{throw 1}catch(a){(function(a){with(o){print(a)}})(2)}"))
      #;(test-case "with shadowing of lexical shadowing of let"
        (check-output '("true")
                      ))
      (test-case "catch shadowing of lexical shadowing of with"
        (check-output '("true")
                      "var o = {a:1};"
                      "with(o){(function(a){try{throw true}catch(a){print(a)}})(2)}"))
      #;(test-case "let shadowing of lexical shadowing of with"
        (check-output '("true")
                      ))
      (test-case "mutation of with-bound variable"
        (check-output '("true")
                      "var o = {a:false}"
                      "with(o) {o.a=true;print(a)}"))
      (test-case "temporary with shadowing of top-level"
        (check-output '("true")
                      "var a = true;"
                      "var o = {a:false}"
                      "with(o) {delete o.a;print(a)}"))
      (test-case "temporary with shadowing of lexical"
        (check-output '("true")
                      "var o = {a:false};"
                      "(function(a){with(o){delete o.a;print(a)}})(true);"))
      (test-case "temporary with shadowing of with"
        (check-output '("true")
                      "var o1 = {a:true};"
                      "var o2 = {a:false};"
                      "with(o1){with(o2){delete o2.a;print(a)}}"))
      (test-case "temporary with shadowing of catch"
        (check-output '("true")
                      "var o = {a:false};"
                      "try{throw true}catch(a){with(o){delete o.a;print(a)}}"))
      #;(test-case "temporary with shadowing of let"
        (check-output '("true")
                      ))
      (test-case "lexical shadowing of temporary with"
        (check-output '("true")
                      "var o = {a:false};"
                      "with(o){(function(a){delete o.a;print(a)})(true)}"))
      (test-case "catch shadowing of temporary with"
        (check-output '("true")
                      "var o = {a:false};"
                      "with(o){try{throw true}catch(a){delete o.a;print(a)}}"))
      #;(test-case "let shadowing of temporary with"
        (check-output '("true")
                      ))
      (test-case "temporary with shadowing of lexical shadowing of with"
        (check-output '("true")
                      "var o1 = {a:1};"
                      "var o2 = {a:2};"
                      "with(o1){(function(a){with(o2){delete o2.a;print(a)}})(true)}"))
      (test-case "temporary with shadowing of catch shadowing of with"
        (check-output '("true")
                      "var o1 = {a:1};"
                      "var o2 = {a:2};"
                      "with(o1){try{throw true}catch(a){with(o2){delete o2.a;print(a)}}}"))
      (test-case "with non-shadowing of lexical shadowing of with"
        (check-output '("true")
                      "var o1 = {a:false};"
                      "var o2 = {};"
                      "with(o1){(function(a){with(o2){print(a)}})(true)}"))
      (test-case "with non-shadowing of catch shadowing of with"
        (check-output '("true")
                      "var o1 = {a:false};"
                      "var o2 = {};"
                      "with(o1){try{throw true}catch(a){with(o2){print(a)}}}"))
      (test-case "with non-shadowing of let shadowing of with"
        (check-output '("true")
                      "var o1 = {a:false};"
                      "var o2 = {};"
                      "with(o1){let(a=true){with(o2){print(a)}}}"))
      ))

  (define prototype-tests
    (test-suite "prototype tests"
      (test-case "global object toString"
        (before
         (reset-javascript-namespace! test-ns)
         (check-equal? (eval-javascript-string "this.toString()" test-ns) "[object DrScheme]")))
      (test-case "vanilla object toString"
        (before
         (reset-javascript-namespace! test-ns)
         (check-equal? (eval-javascript-string "({}).toString()" test-ns) "[object Object]")))
      (test-case "dot method call sets up current this"
        (check-output '("true")
                      "({foo:function(){print(this.bar)},bar:true}).foo()"))
      (test-case "eval maintains current this"
        (check-output '("true")
                      "({foo:function(){eval('print(this.bar)')},bar:true}).foo()"))
      (test-case "eval gets same namespace"
        (check-output '("true")
                      "var a = true;"
                      "eval('print(this.a)');"))
      ))

  (define library-tests
    (test-suite "library tests"
      (test-case "String called as a function"
        (before
         (reset-javascript-namespace! test-ns)
         (check-equal? (eval-javascript-string "String(44)" test-ns) "44")))
      (test-case "Number called as a function"
        (before
         (reset-javascript-namespace! test-ns)
         (check-equal? (eval-javascript-string "Number('44')" test-ns) 44)))
      (test-case "Boolean called as a function"
        (before
         (reset-javascript-namespace! test-ns)
         (check-equal? (eval-javascript-string "Boolean(0)" test-ns) 'false)))
      (test-case "String.fromCharCode"
        (before
         (reset-javascript-namespace! test-ns)
         (check-equal? (eval-javascript-string "String.fromCharCode(104,101,108,108,111)" test-ns) "hello")))
      (test-case "Array with 0 args"
        (check-output '("")
                      "var a = new Array(); print(a)"))
      (test-case "Array with 1 arg - number"
        (check-output '(",,,,")
                      "var a = new Array(5); print(a)"))
      (test-case "Array with 1 arg - non-number"
        (check-output '("true")
                      "var a = new Array(true); print(a)"))
      (test-case "Array with two args"
        (check-output '("1,2")
                      "var a = new Array(1,2); print(a)"))
      (test-case "Array with three args"
        (check-output '("1,2,3")
                      "var a = new Array(1,2,3); print(a)"))
      ))

  (define eval-tests
    (test-suite "eval tests"
      binding-tests
      prototype-tests
      library-tests
      ))

  (provide eval-tests))
