(module settings mzscheme
  (require (planet "contract-utils.ss" ("cobbe" "contract-utils.plt" 1 0))
           (lib "contract.ss")
           (lib "serialize.ss")
           (lib "struct.ss"))
  (require-for-syntax (lib "struct.ss" "syntax"))

  (define-syntax (provide-settings stx)
    (syntax-case stx ()
      [(_ (settings-struct current-settings default-settings) clause ...)
       (with-syntax ([(field-name ...) (map (lambda (clause-stx)
                                              (syntax-case clause-stx ()
                                                [(name default . _) #'name]))
                                            (syntax->list #'(clause ...)))]
                     [(field-default ...) (map (lambda (clause-stx)
                                                 (syntax-case clause-stx ()
                                                   [(name default . _) #'default]))
                                               (syntax->list #'(clause ...)))]
                     [(field-contract ...) (map (lambda (clause-stx)
                                                  (syntax-case clause-stx ()
                                                    [(name default) #'any/c]
                                                    [(name default contract . _) #'contract]))
                                                (syntax->list #'(clause ...)))]
                     [(field-guard ...) (map (lambda (clause-stx)
                                               (syntax-case clause-stx ()
                                                 [(name default contract guard) #'guard]
                                                 [_ #'#f]))
                                             (syntax->list #'(clause ...)))])
         (with-syntax ([(struct:settings make-settings settings? get-field ...)
                        (build-struct-names
                         #'settings-struct
                         (syntax->list #'(field-name ...))
                         #f
                         #t
                         stx)])
           #'(begin
               (define-serializable-struct settings-struct (field-name ...) #f)
               (define default-settings (make-settings field-default ...))
               (define current-settings (make-parameter default-settings))
               (define field-name
                 (let ([g field-guard])
                   (case-lambda
                     [() (get-field (current-settings))]
                     [(value)
                      (when g
                        (g value))
                      (current-settings
                       (copy-struct settings-struct (current-settings) [get-field value]))])))
               ...
               (provide/contract (struct settings-struct ([field-name field-contract] ...))
                                 [default-settings settings?]
                                 [current-settings parameter?]
                                 [field-name (case->
                                              (-> field-contract)
                                              (field-contract . -> . any))]
                                 ...))))]))

  (define (marshall-settings settings)
    (serialize settings))

  (define (unmarshall-settings settings)
    (deserialize settings))

  (provide provide-settings marshall-settings unmarshall-settings))
