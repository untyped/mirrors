(module exn mzscheme

  (define-struct (exn:fail:javascript exn:fail) ())
  (define-struct (exn:fail:javascript:syntax exn:fail:javascript) (source location text) #f)
  (define-struct (exn:fail:javascript:runtime exn:fail:javascript) (value) #f)

  (provide (all-defined)))
