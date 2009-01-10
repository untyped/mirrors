(module pretty-print mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 1))
           (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 2 1))
           "../../syntax/pretty-print.ss"
           "../../syntax/parse.ss"
           "../../syntax/sexp.ss"
           "../../syntax/ast.ss")

  ;; TODO: test expression statement never generates immediate function subexpression

  (define (pretty-test term)
    (printf "~v~n" (pretty-format term)))

  (define-check (check-expression sexp)
    (check-equal? sexp
                  (Expression->sexp (parse-expression (pretty-format (sexp->Expression sexp))))))

  (define-check (check-source-element sexp)
    (check-equal? sexp
                  (SourceElement->sexp (parse-source-element (pretty-format (sexp->SourceElement sexp))))))

  (define invertability-tests
    (test-suite "tests of invertability of pretty-printing"
      (test-case "array literal"
        (check-expression '(array 1 2 3 4 5)))
      (test-case "sparse array literal"
        (check-expression '(array 1 () 2 () 3)))
      (test-case "function"
        (check-source-element '(function f (a b c)
                                 (return a))))
      (test-case "object literal"
        (check-expression '(object [a "aaa"]
                                   [b 12]
                                   ["is" 42])))
      (test-case "block statement"
        (check-source-element '(block "foo" (print 6))))
      (test-case "one-armed if"
        (check-source-element '(if #f (print 12))))
      (test-case "nested ifs"
        (check-source-element '(if #f
                                   (block (print 2))
                                   (if #f
                                       (print 3)
                                       (if #t
                                           (print)
                                           (print null))))))
      (test-case "empty do body"
        (check-source-element '(do () #f)))
      (test-case "do with block body"
        (check-source-element '(do (block (break)) #f)))
      (test-case "for-in with var declaration"
        (check-source-element '(for-in ((var x) (array 0 1 2))
                                 (block (print x)))))
      (test-case "application of complex expression"
        (check-expression '((new Function "print('hi!');"))))
      (test-case "nested addition"
        (check-expression '(+ (+ 2 3) 4)))
      (test-case "order of operations 1"
        (check-expression '(* (+ 2 3) 4)))
      (test-case "order of operations 2"
        (check-expression '(+ (* 2 3) 4)))
      (test-case "order of operations 3"
        (check-expression '(+ 2 (* 3 4))))
      (test-case "nested addition with different kinds of operands"
        (check-expression '(+ "foo" (+ 2 3))))
      (test-case "order of operations with overloaded addition"
        (check-expression '(+ "foo" (* 2 3))))
      (test-case "nested functions"
        (check-expression '(function ()
                             (function foo () (return))
                             (foo))))
      (test-case "empty for init"
        (check-source-element '(for #f #t #f (break))))
      (test-case "standard for loop"
        (check-source-element '(for ((= i 0)) (< i 10) ((postfix i ++)) (print i))))
      (test-case "complex for loop"
        (check-source-element '(for (var [i 0] [j 10]) (< i 10) (begin (postfix i ++) (postfix j --))
                                 (print (+ i (+ ", " j))))))
      ))

  (define pretty-print-tests
    (test-suite "pretty-print tests"
     invertability-tests
     ))

  (provide pretty-print-tests))
