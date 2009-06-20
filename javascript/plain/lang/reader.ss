(module reader syntax/module-reader
  #:language `(planet "module.ss" ("untyped" "mirrors.plt" 2) "javascript/plain")
  #:read
  (lambda ([in (current-input-port)])
    (let ([ast (with-syntax-errors (lambda () (parse-program-unit in)))])
      (list `(#%module-begin ,@ast))))
  #:read-syntax
  (lambda ([source-name #f] [in (current-input-port)])
    (let ([ast (with-syntax-errors (lambda () (parse-program-unit in)))])
      (list `(#%module-begin ,@ast))))
  #:whole-body-readers? #t
  (require "../../../base.ss")
  (require (javascript-in private/compiler/compile
                          private/syntax/parse)))