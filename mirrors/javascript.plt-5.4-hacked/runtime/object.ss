(module object mzscheme
  (require (planet "evector.scm" ("soegaard" "evector.plt" 1 0))
           (lib "etc.ss"))
           
  (require-for-syntax (lib "etc.ss")
                      (lib "stx.ss" "syntax"))

  ;; empty-bit-field : bit-field
  (define empty-bit-field 0)

  ;; empty-bit-field? : bit-field -> boolean
  (define empty-bit-field? zero?)

  ;; bit-field : bit-flag ... -> bit-field
  (define (bit-field . flags)
    (if (null? flags) 0 (apply bitwise-ior flags)))

  ;; make-bit-field : (cons bit-flag boolean) ... -> bit-field
  (define (make-bit-field . pairs)
    (apply bit-field (map (lambda (pair)
                            (if (cdr pair) (car pair) 0))
                          pairs)))

  (define-syntax (define-bit-flags stx)
    (syntax-case stx ()
      [(_ (x1 x2 ...))
       (with-syntax ([(i1 i2 ...) (build-list (length (syntax->list #'(x1 x2 ...)))
                                              (lambda (i) (expt 2 i)))])
         #'(begin (define x1 i1)
                  (define x2 i2)
                  ...))]))

  ;; bit-flag-set? : bit-field bit-flag -> boolean
  (define (bit-flag-set? bf flag)
    (not (zero? (bitwise-and bf flag))))

  (define-syntax (object-table stx)
    (syntax-case stx ()
      [(_ p ...)
       (with-syntax ([(property ...)
                      (map (lambda (stx)
                             (with-syntax ([name-str (let ([x (syntax-object->datum (stx-car stx))])
                                                       (if (string? x)
                                                           x
                                                           (symbol->string x)))])
                               (syntax-case stx ()
                                 [[name value]
                                  #'[name-str value]]
                                 [[name value (attributes ...)]
                                  #'[name-str (cons value (bit-field attributes ...))]]
                                 [[name getter setter (attributes ...)]
                                  #'[name-str (cons (make-ref getter setter (lambda () 'false))
                                                    (bit-field DONT-DELETE? attributes ...))]])))
                           (syntax->list #'(p ...)))])
         #'(hash-table 'equal property ...))]))

  ;; CONVENTION: Never allow a closure to close over a binding to an object's
  ;;             property table; this may prevent garbage collection of an object's
  ;;             properties if its table is replaced...?
  (define-struct object (call construct proto class properties) #f)
  (define-struct (array object) (vector) #f)
  (define-struct ref (get set! delete!))

  (define (function? x)
    (and (object? x)
         (object-call x)
         #t))

  (define-bit-flags (READ-ONLY? DONT-ENUM? DONT-DELETE?))

  (provide (all-defined)))
