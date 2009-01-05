(module test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 1))
           (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 2 1))
           "pretty-print.ss"
           "parse.ss"
           "eval.ss")

  (define all-tests
    (test-suite "JavaScript Test Suite"
     parse-tests
     pretty-print-tests
     eval-tests
     ))

  (print-struct #t)
  (test/graphical-ui all-tests)

  (provide all-tests))
