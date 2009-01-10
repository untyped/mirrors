(module lexical-context mzscheme
  (require (planet "evector.scm" ("soegaard" "evector.plt" 1 0))
           (file "../../list.plt-2.0-hacked/list.ss")
           (lib "etc.ss")
           "../syntax/ast.ss"
           "../runtime/runtime.ss"
           "helpers.ss")

  ;; static-environment : (parameterof (alistof Identifier (optional arguments-alias)))
  (define static-environment (make-parameter null))

  (define current-with-statement (make-parameter #f))

  ;; The (potentially dynamic) scope chain, only used underneath `with'.
  (define scope-chain (datum->syntax-object #f 'scope-chain))

  (define current-labels (make-parameter null))

  (define enable-return? (make-parameter #f))

  (define-struct arguments-alias (vector-id offset) #f)

  ;; extend-static-env : (listof Identifier) * (listof (optional syntax)) * env -> env
  (define (extend-static-env ids refs env)
    (append (map cons ids refs) env))

  ;; make-bindings : (listof symbol) * [(optional region) * (optional (listof (union arguments-alias syntax)))]
  (define make-bindings
    (opt-lambda (names [loc #f] [bindings #f])
      (let* ([ids (map (lambda (name)
                         (if (symbol? name)
                             (make-Identifier loc name)
                             name))
                       names)]
             [stx-ids (map Identifier->syntax ids)]
             [static-bindings (if (not bindings)
                                  (map (lambda (id) #f) stx-ids)
                                  (map (lambda (binding)
                                         (and (arguments-alias? binding) binding))
                                       bindings))]
             [extend (lambda (env)
                       (extend-static-env ids static-bindings env))])
        (values stx-ids
                (cond
                  [(and (current-with-statement) bindings)
                   (lambda (body)
                     (with-syntax ([scope-chain scope-chain]
                                   [(prop ...)
                                    (map (lambda (id binding)
                                           (if (arguments-alias? binding)
                                               (with-syntax ([v id]
                                                             [vec (arguments-alias-vector-id binding)]
                                                             [i (arguments-alias-offset binding)]
                                                             [(val) (generate-temporaries '(val))])
                                                 #'[v (lambda ()
                                                        (evector-ref vec i))
                                                      (lambda (val)
                                                        (evector-set! vec i val))
                                                      ()])
                                               (with-syntax ([v id]
                                                             [val binding])
                                                 #'[v val])))
                                         stx-ids
                                         bindings)]
;                                   [(v ...) stx-ids]
;                                   [(vec ...) (map arguments-alias-vector-id aliases)]
;                                   [(i ...) (map arguments-alias-offset aliases)]
                                   [body body])
                       (syntax/loc (region->syntax loc)
                         (let ([scope-chain (cons (make-frame (object-table prop ...)) scope-chain)])
                           body))))]

;                         (let ([scope-chain (cons (make-frame (arguments-frame-table [v vec i] ...)) scope-chain)])
;                           body))))]
                  [(current-with-statement)
                   (lambda (body)
                     (with-syntax ([scope-chain scope-chain]
                                   [(v ...) stx-ids]
                                   [(key ...) (map (compose symbol->string syntax-object->datum) stx-ids)]
                                   [body body])
                       (syntax/loc (region->syntax loc)
                         (let ([scope-chain (cons (make-frame (object-table [v (void)] ...)) scope-chain)])
                           body))))]
                  [bindings
                   (lambda (body)
                     (with-syntax ([(vb ...) (filter-map (lambda (id binding)
                                                           (and (syntax? binding) id))
                                                         stx-ids bindings)]
                                   [(e ...) (filter-map (lambda (binding)
                                                          (and (syntax? binding) binding))
                                                        bindings)]
                                   [(va ...) (filter-map (lambda (id binding)
                                                           (and (arguments-alias? binding) id))
                                                         stx-ids bindings)]
                                   [(vec ...) (filter-map (lambda (binding)
                                                            (and (arguments-alias? binding)
                                                                 (arguments-alias-vector-id binding)))
                                                          bindings)]
                                   [(i ...) (filter-map (lambda (binding)
                                                          (and (arguments-alias? binding)
                                                               (arguments-alias-offset binding)))
                                                        bindings)]
                                   [body body])
                       (syntax/loc (region->syntax loc)
                         ;; NOTE: We put the let-bindings outside since the let-syntax bindings
                         ;;       can't accidentally refer to the let-bound variables, but the
                         ;;       reverse is not necessarily true.
                         (let ([vb e] ...)
                           (let-syntax ([va (syntax-id-rules (set!)
                                              [(set! v expr) (evector-set! vec i expr)]
                                              [va (evector-ref vec i)])]
                                        ...)
                             body)))))]
;                     (with-syntax ([(v ...) stx-ids]
;                                   [(vec ...) (map arguments-alias-vector-id aliases)]
;                                   [(i ...) (map arguments-alias-offset aliases)]
;                                   [body body])
;                       (syntax/loc (region->syntax loc)
;                         (let-syntax ([v (syntax-id-rules (set!)
;                                           [(set! v expr) (evector-set! vec i expr)]
;                                           [v (evector-ref vec i)])]
;                                      ...)
;                           body))))]
                  [else
                   (lambda (body)
                     (with-syntax ([(v ...) stx-ids]
                                   [body body])
                       (syntax/loc (region->syntax loc)
                         (let ([v (void)] ...)
                           body))))])
                extend))))

  (provide (all-defined)))
