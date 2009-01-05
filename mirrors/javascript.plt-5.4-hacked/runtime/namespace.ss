(module namespace mzscheme
  (require (lib "mred.ss" "mred")
           (lib "etc.ss")
           ;; These two are required "in spirit":
           "runtime.ss"
           "standard-library.ss")

  ;; TODO: remove the mred dependency and move all that stuff into tool.ss

  (define (require-spec module-name . path)
    `(file ,(path->string (apply build-path (this-expression-source-directory) 'up (append path (list module-name))))))

  (define runtime (require-spec "runtime.ss" "runtime"))
  (define standard-library (require-spec "standard-library.ss" "runtime"))
  (define debug (require-spec "debug.ss"))

  ;; make-javascript-namespace : -> javascript-namespace
  (define (make-javascript-namespace)
    (let ([ns (make-gui-namespace)])
      (parameterize ([current-namespace ns])
        (dynamic-require runtime #f)
        (dynamic-require standard-library #f)
        (dynamic-require debug #f)
        (let ([path1 ((current-module-name-resolver) runtime #f #f)]
              [path2 ((current-module-name-resolver) standard-library #f #f)]
              [path3 ((current-module-name-resolver) debug #f #f)])
          (namespace-attach-module ns path1)
          (namespace-attach-module ns path2)
          (namespace-attach-module ns path3)))
      (reset-javascript-namespace! ns)
      ns))

  ;; reset-javascript-namespace! : javascript-namespace -> any
  (define (reset-javascript-namespace! ns)
    (parameterize ([current-namespace ns])
      (let ([global-object (dynamic-require runtime 'global-object)]
            [install-standard-library! (dynamic-require standard-library 'install-standard-library!)])
        (install-standard-library! global-object)))
    (void))

  (provide make-javascript-namespace reset-javascript-namespace!))
