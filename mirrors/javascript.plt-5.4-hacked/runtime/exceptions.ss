(module exceptions mzscheme
  (require "../exn.ss")

  ;; TODO: create a current-location syntax-parameter

  (define-syntax (raise-runtime-exception stx)
    (syntax-case stx ()
      [(_ loc e)
       (with-syntax ([c-c-m (syntax/loc #'loc (current-continuation-marks))])
         #'(raise (make-exn:fail:javascript:runtime (string->immutable-string "uncaught exception")
                                                    c-c-m
                                                    e)))]
      ))

  (define-syntax raise-reference-error
    (syntax-rules ()
      [(_ loc e)
       (let ([message (format "~a is not defined" e)])
         (raise-runtime-exception loc message))]))

  (define-syntax raise-runtime-type-error
    (syntax-rules ()
      [(_ loc expected actual)
       (let ([message (format "expected ~a, received ~a" expected actual)])
         (raise-runtime-exception loc message))]))

  ;; TODO: make an object of the appropriate type
  ;; TODO: include the source code
  (define-syntax raise-assignment-error
    (syntax-rules ()
      [(_ loc)
       (let ([message "invalid assignment left-hand-side"])
         (raise-runtime-exception loc message))]))

  (provide (all-defined)))
