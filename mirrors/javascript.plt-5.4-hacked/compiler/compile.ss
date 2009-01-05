(module compile mzscheme
  (require (planet "evector.scm" ("soegaard" "evector.plt" 1 0))
           (file "../../list.plt-2.0-hacked/list.ss")
           (lib "match.ss")
           (lib "etc.ss")
           "../syntax/ast.ss"
           "../syntax/token.ss"
           "../config.ss"
           "../exn.ss"
           "../runtime/runtime.ss"
           "../debug.ss"
           "helpers.ss"
           "hoist.ss"
           "lexical-context.ss")

  ;; TODO: is generate-temporaries guaranteed never to be captured?
  ;;  - perhaps not, because sometimes 'x57 == (generate-temporaries '(x))
  ;;  - perhaps so, if we always use non-forgeable names (e.g., with an illegal character)
  ;;  - but be careful about capture with string keys!

  ;; TODO: optimizations
  ;;   - don't capture return continuation if it's not used
  ;;   - remove unnecessary `void' if last statement in function is not an expression statement

  (define-syntax syntax/loc*
    (syntax-rules ()
      [(_ loc expr)
       (syntax/loc (region->syntax loc)
         expr)]))

  ;; loop? : Statement -> boolean
  (define (loop? stmt)
    (or (DoWhileStatement? stmt)
        (WhileStatement? stmt)
        (ForStatement? stmt)
        (ForInStatement? stmt)))

  ;; ===========================================================================
  ;; COMPILER TOP-LEVEL
  ;; ===========================================================================

  (define (with-syntax-errors thunk)
    (with-handlers ([exn:fail:javascript:syntax?
                     (lambda (exn)
                       (let* ([loc (exn:fail:javascript:syntax-location exn)]
                              [text (format "~a" (exn:fail:javascript:syntax-text exn))]
                              [stxloc (build-syntax (string->symbol text) loc)])
                         (raise-syntax-error 'parse (exn-message exn) stxloc stxloc)))])
      (thunk)))

  ;; compile-script : (listof SourceElement) -> syntax
  (define (compile-script elts)
    (let*-values ([(funs vars stmts) (hoist-script elts)]
                  [(definitions new-env) (compile-declarations funs vars)])
      (with-syntax ([(defn ...) definitions]
                    [scope-chain scope-chain]
                    [(s ...) (parameterize ([static-environment new-env])
                               (map compile-statement stmts))])
        #'(parameterize ([current-completion #f])
            (define scope-chain null)
            defn ... s ...
            (current-completion)))))

  ;; compile-interaction : (listof SourceElement) -> syntax
  (define (compile-interaction elt)
    (let*-values ([(funs vars stmts) (hoist-script elt)]
                  [(definitions new-env) (compile-declarations funs vars)])
      (with-syntax ([(defn ...) definitions]
                    [scope-chain scope-chain]
                    [(s ...) (parameterize ([static-environment new-env])
                               (map compile-statement stmts))]
                    [(previous-completion) (generate-temporaries '(previous-completion))])
        (static-environment new-env)
        #'(begin
            (define previous-completion (current-completion))
            (current-completion #f)
            (define scope-chain null)
            defn ... s ...
            (begin0
              (cond
                [(current-completion)
                 => (lambda (v)
                      (object-set! global-object "it" v)
                      v)]
                [else #f])
              (current-completion previous-completion))))))

  ;; compile-function-expression : FunctionExpression -> syntax
  (define (compile-function-expression expr)
    (match (hoist-expression expr)
      [($ FunctionExpression/hoisted loc name args body funs vars)
       (compile-function loc name args body funs vars)]))

  ;; compile-declarations : (listof FunctionDeclaration/hoisted) * (listof Identifier)
  ;;                     -> (listof syntax)
  ;;                        environment
  ;; compiles top-level declarations ONLY
  (define (compile-declarations funs vars)
    (let* ([fun-ids (map FunctionDeclaration-name funs)]
           [all-ids (append fun-ids vars)]
           [new-env (append (map (lambda (id) (cons id #f)) all-ids)
                            (static-environment))]
           [definitions (with-syntax ([(var ...) (map Identifier->syntax all-ids)]
                                      [(var-key ...) (map Identifier->key all-ids)]
                                      [(init-e ...) (append (parameterize ([static-environment new-env])
                                                              (map compile-function-declaration funs))
                                                            (map (lambda (var) #'(void)) vars))])
                          (syntax->list #'((define-syntax var
                                             (syntax-id-rules (set!)
                                               [(set! var expr) (object-put! global-object var-key expr)]
                                               ;; TODO: does this object-get need to be guarded?
                                               [var (object-get global-object var-key)]))
                                           ...
                                           (set! var init-e) ...)))])
;        (for-each (lambda (defn)
;                    (fprintf (current-error-port) "defining: ~a~n" (syntax-object->datum defn)))
;                  definitions)
        (values definitions new-env)))

  ;; ===========================================================================
  ;; LVALUES
  ;; ===========================================================================

  ;; reference-expression? : Expression -> boolean
  (define (reference-expression? expr)
    (or (VarReference? expr)
        (BracketReference? expr)
        (DotReference? expr)))

  ;; compile-deletion : Expression -> syntax
  (define (compile-deletion expr)
    (match expr
      [($ VarReference loc id)
       (cond
         [(and (not (current-with-statement))
               (not (s:assoc id (static-environment) Identifier=?)))
          #'(quote true)]
         [(current-with-statement)
          (with-syntax ([scope-chain scope-chain]
                        [key (Identifier->key id)])
            (syntax/loc (region->syntax loc)
              (scope-chain-delete! scope-chain key)))]
         [else
          #'(quote false)])]
      [($ BracketReference loc container key)
       (with-syntax ([container-e (compile-expression container)]
                     [key-e (compile-expression key)]
                     [(obj-val) (generate-temporaries '(obj-val))])
         (syntax/loc (region->syntax loc)
           (let ([obj-val (value->object container-e)])
             (object-delete! obj-val key-e))))]
      [($ DotReference loc container id)
       (with-syntax ([container-e (compile-expression container)]
                     [key-e (Identifier->key id)]
                     [(obj-val) (generate-temporaries '(obj-val))])
         (syntax/loc (region->syntax loc)
           (let ([obj-val (value->object container-e)])
             (object-delete! obj-val key-e))))]))

  ;; compile-assignment : Expression * syntax -> syntax
  (define (compile-assignment lhs rhs-stx)
    (match lhs
      [($ VarReference loc id)
       (debug 'scope-resolution "looking for ~a in ~v" (Identifier-name id) (static-environment))
       (cond
         [(and (not (current-with-statement))
               (not (s:assoc id (static-environment) Identifier=?)))
          (debug 'unbound-reference "~a unbound at ~a" (Identifier-name id) (region->string loc))
          (with-syntax ([key (Identifier->key id)]
                        [rhs-e rhs-stx]
                        [(val) (generate-temporaries '(val))])
            (syntax/loc (region->syntax loc)
              (let ([val rhs-e])
                (object-put! global-object key val)
                val)))]
         [(current-with-statement)
          (with-syntax ([scope-chain scope-chain]
                        [key (Identifier->key id)]
                        [rhs-e rhs-stx])
            (syntax/loc (region->syntax loc)
              (scope-chain-set! scope-chain key rhs-e)))]
         [else
          (with-syntax ([x (Identifier->syntax id)]
                        [(val) (generate-temporaries '(val))]
                        [rhs-e rhs-stx])
            (syntax/loc (region->syntax loc)
              (let ([val rhs-e])
                (set! x val)
                val)))])]
      [($ BracketReference loc container key)
       (with-syntax ([container-e (compile-expression container)]
                     [key-e (compile-expression key)]
                     [rhs-e rhs-stx]
                     [(container-val key-val) (generate-temporaries '(container-val key-val))])
         (syntax/loc (region->syntax loc)
           (let* ([container-val (value->object container-e)]
                  [key-val key-e])
             (object-set! container-val key-val rhs-e))))]
      [($ DotReference loc container id)
       (with-syntax ([container-e (compile-expression container)]
                     [rhs-e rhs-stx]
                     [key-val (Identifier->key id)])
         (syntax/loc (region->syntax loc)
           (let ([container-val (value->object container-e)])
             (object-set! container-val key-val rhs-e))))]
      [_ (raise-syntax-error 'compile "invalid assignment left-hand side" (region->syntax (Term-location lhs)))]))

  ;; compile-lookup : reference-expression -> syntax
  (define (compile-lookup expr)
    (match expr
      ;; TODO: binding forms should still introduce binding arrows under with
      ;;  - add name to static environment
      ;;  - compile ref to (if #f x (dynamic-lookup 'x))
      [($ VarReference loc id)
       (debug 'scope-resolution "looking for ~a in ~v" (Identifier-name id) (static-environment))
       (cond
         [(and (not (current-with-statement))
               (not (s:assoc id (static-environment) Identifier=?)))
          (debug 'unbound-reference "~a unbound at ~a" (Identifier-name id) (region->string loc))
          (with-syntax ([stxloc (region->syntax loc)]
                        [key (Identifier->key id)])
            (syntax/loc (region->syntax loc)
              (or (object-get global-object key)
                  (raise (make-exn:fail:contract:variable (string->immutable-string (format "~a is not defined" key))
                                                          (current-continuation-marks)
                                                          (string->symbol key))))))]
         [(current-with-statement)
          (with-syntax ([scope-chain scope-chain]
                        [key (Identifier->key id)])
            (syntax/loc (region->syntax loc)
              (or (scope-chain-get scope-chain key)
                  (raise (make-exn:fail:contract:variable (string->immutable-string (format "~a is not defined" key))
                                                          (current-continuation-marks)
                                                          (string->symbol key))))))]
         [else (Identifier->syntax id)])]
      [($ BracketReference loc container key)
       (with-syntax ([container-e (compile-expression container)]
                     [key-e (compile-expression key)]
                     [(container-val) (generate-temporaries '(container-val))])
         (syntax/loc (region->syntax loc)
           (let ([container-val (value->object container-e)])
             (or (object-get container-val key-e) (void)))))]
      [($ DotReference loc container id)
       (with-syntax ([container-e (compile-expression container)]
                     [key-val (Identifier->key id)])
         (syntax/loc (region->syntax loc)
           (or (object-get container-e key-val) (void))))]))

  ;; ===========================================================================
  ;; COMPILER CORE
  ;; ===========================================================================

  (define (compile-function-declaration decl)
    (match decl
      [($ FunctionDeclaration/hoisted loc name args body funs vars)
       (compile-function loc name args body funs vars)]))

  ;; TODO: handle return more gracefully (particularly invalid returns)

  (define (compile-function loc name args body funs vars)
    (let ([arity (length args)])
      (with-syntax ([(func-object arg-vec args-object) (generate-temporaries '(func-object arg-vec args-object))])
        (let ([aliases (build-list arity (lambda (i)
                                           (make-arguments-alias #'arg-vec i)))])
          (let-values ([(arg-stx-ids add-args bind-args) (make-bindings args loc aliases)]
                       [(fun-stx-ids add-funs bind-funs) (make-bindings (map FunctionDeclaration-name funs) loc)]
                       [(var-stx-ids add-vars bind-vars) (make-bindings (cons (make-Identifier loc 'arguments) vars) loc)]
                       [(name-stx-id add-name bind-name) (make-bindings (if name (list name) null) loc)])
            (let ([new-static-env (bind-name (bind-args (bind-funs (bind-vars (static-environment)))))])
              (with-syntax ([return (datum->syntax-object #f 'return)]
                            [(s ...) (parameterize ([enable-return? #t]
                                                    [static-environment new-static-env])
                                       (map compile-statement body))]
                            [set-arguments! (compile-assignment (make-VarReference loc (make-Identifier loc 'arguments)) #'args-object)]
                            [(set-nested-func! ...)
                             (parameterize ([static-environment new-static-env])
                               (map (lambda (fun)
                                      (compile-assignment (make-VarReference (Term-location fun) (FunctionDeclaration-name fun))
                                                          (compile-function-declaration fun)))
                                    funs))])
                (with-syntax ([body (quasisyntax/loc (region->syntax loc)
                                      (lambda (arg-vec)
                                        (let ([args-object (make-arguments-object func-object arg-vec)])
                                          #,(add-args
                                             (add-funs
                                              (add-vars
                                               (syntax/loc (region->syntax loc)
                                                 (begin
                                                   set-arguments!
                                                   set-nested-func! ...
                                                   (parameterize ([current-completion #f])
                                                     (let/ec return
                                                       s ...
                                                       (void)))))))))))]
                              [arity arity])
                  (if name
                      (with-syntax ([set-f! (parameterize ([static-environment new-static-env])
                                              (compile-assignment (make-VarReference loc name)
                                                                  #'func-object))])
                        (add-name
                         (syntax/loc (region->syntax loc)
                           (letrec ([func-object (build-function arity body)])
                             set-f!
                             func-object))))
                      (syntax/loc (region->syntax loc)
                        (letrec ([func-object (build-function arity body)])
                          func-object)))))))))))

  (define (compile-statement stmt)
    (match stmt
      [($ BlockStatement/hoisted loc stmts funs vars)
       (let-values ([(var-stx-ids add-args bind-args) (make-bindings vars loc)]
                    [(fun-stx-ids add-funs bind-funs) (make-bindings (map FunctionDeclaration-name funs) loc)])
         (let ([new-static-env (bind-args (bind-funs (static-environment)))])
           (with-syntax ([(f ...) fun-stx-ids]
                         [(fe ...) (parameterize ([static-environment new-static-env])
                                     (map compile-function-declaration funs))]
                         [(s ...) (parameterize ([static-environment new-static-env])
                                    (map compile-statement stmts))])
             (add-args
              (add-funs
               (syntax/loc (region->syntax loc)
                 (begin
                   (set! f fe) ...
                   s ...
                   (current-completion))))))))]
      [($ EmptyStatement loc)
       (syntax/loc* loc
         #f)]
      [($ ExpressionStatement loc expr)
       (with-syntax ([e (compile-expression expr)])
         (syntax/loc* loc
           (complete! e)))]
      ;; TODO: test IfStatement
      [($ IfStatement loc test consequent alternate)
       (with-syntax ([test-e (compile-expression test)]
                     [consequent-s (compile-statement consequent)]
                     [alternate-s (if alternate (compile-statement alternate) #'#f)])
         (syntax/loc* loc
           (if (true-value? test-e)
               consequent-s
               alternate-s)))]
      ;; TODO: test loops
      [(? loop?)
       (with-syntax ([(break continue) (generate-temporaries '(break continue))])
         (parameterize ([current-labels (cons (list #f #'break #'continue)
                                              (current-labels))])
           (compile-loop stmt #'break #'continue)))]
      ;; TODO: test ContinueStatement
      [($ ContinueStatement loc #f)
       (cond
         [(ormap (lambda (tuple)
                   (and (pair? (cddr tuple))
                        (caddr tuple)))
                 (current-labels))
          => (lambda (continue-id)
               (with-syntax ([continue continue-id])
                 (syntax/loc* loc
                   (continue #f))))]
         [else (let ([stxloc (build-syntax 'continue loc)])
                 (raise-syntax-error 'continue "invalid continue" stxloc stxloc))])]
      [($ ContinueStatement loc label)
       (cond
         [(null? (current-labels))
          (raise-syntax-error 'continue "invalid continue" (build-syntax 'continue loc))]
         [(assq (Identifier-name label) (current-labels))
          => (lambda (tuple)
               (if (pair? (cddr tuple))
                   (with-syntax ([continue (caddr tuple)])
                     (syntax/loc* loc
                       (continue #f)))
                   (raise-syntax-error 'continue "invalid label" (Identifier->syntax label))))]
         [else (raise-syntax-error 'continue "invalid label" (Identifier->syntax label))])]
      ;; TODO: test BreakStatement
      [($ BreakStatement loc #f)
       (when (null? (current-labels))
         (let ([stxloc (build-syntax 'break loc)])
           (raise-syntax-error 'break "invalid break" stxloc stxloc)))
       (with-syntax ([break (cadar (current-labels))])
         (syntax/loc* loc
           (break (current-completion))))]
      [($ BreakStatement loc label)
       (cond
         [(null? (current-labels))
          (raise-syntax-error 'break "invalid break" (build-syntax 'break loc))]
         [(assq (Identifier-name label) (current-labels))
          => (lambda (tuple)
               (with-syntax ([break (cadr tuple)])
                 (syntax/loc* loc
                   (break (current-completion)))))]
         [else (raise-syntax-error 'break "invalid label" (Identifier->syntax label))])]
      [($ ReturnStatement loc value)
       (unless (enable-return?)
         (let ([stxloc (build-syntax 'return loc)])
           (raise-syntax-error 'return "invalid return" stxloc stxloc)))
       (with-syntax ([return (datum->syntax-object #f 'return)]
                     [e (if value
                            (compile-expression value)
                            #'(void))])
         (syntax/loc* loc
           (return e)))]
      [($ LetStatement loc bindings body)
       (let-values ([(var-stx-ids add-vars bind-vars) (make-bindings (map VariableInitializer-id bindings)
                                                                     loc
                                                                     (map (compose compile-expression VariableInitializer-init) bindings))])
         (with-syntax ([body (parameterize ([static-environment (bind-vars (static-environment))])
                               (compile-statement body))])
           (add-vars (syntax/loc (region->syntax loc) body))))]
      [($ WithStatement loc object body)
       (let* ([unique-entries (delete-duplicates (static-environment) (lambda (e1 e2)
                                                                        (Identifier=? (car e1) (car e2))))]
              [all-identifiers-in-scope (map car unique-entries)])
         (with-syntax ([scope-chain scope-chain]
                       ;; Shadow all bound variables with expressions that look them up in
                       ;; the dynamic environment, and have their binding arrows point to
                       ;; this with statement.
                       [(shadow-x ...) (map (lambda (id)
                                              (Identifier->syntax id (Term-location object)))
                                            all-identifiers-in-scope)]
                       ;; Uses of these variables will not be associated with source locations.
                       [(invisible-x ...) (map (lambda (id)
                                                 (Identifier->syntax id #f))
                                               all-identifiers-in-scope)]
                       [(x-value ...) (map (lambda (entry)
                                             (let* ([id (car entry)]
                                                    [var (make-VarReference (Term-location id) id)])
                                               (compile-lookup var)))
                                           unique-entries)]
                       [(x-key ...) (map Identifier->key all-identifiers-in-scope)]
                       ;; The expression is evaluated with the current lexical environment.
                       [e (compile-expression object)]
                       ;; The body is evaluated in the syntactic context of the with statement
                       ;; and has an empty initial static environment.
                       [s (parameterize ([static-environment null]
                                         [current-with-statement stmt])
                            (compile-statement body))])
           (with-syntax ([base-chain (if (not (current-with-statement))
                                         #'(list (make-frame (object-table [invisible-x x-value] ...))
                                                 global-object)
                                         #'scope-chain)])
             (syntax/loc (region->syntax loc)
               (let ([scope-chain (cons e base-chain)])
                 (let-syntax ([shadow-x (syntax-id-rules (set!)
                                          [(set! shadow-x expr) (scope-chain-set! scope-chain x-key expr)]
                                          [shadow-x (or (scope-chain-get scope-chain x-key)
                                                        (raise (make-exn:fail:contract:variable
                                                                (string->immutable-string (format "~a is not defined" x-key))
                                                                (current-continuation-marks)
                                                                (string->symbol x-key))))])]
                              ...)
                   s))))))]
      ;; TODO: test SwitchStatement
      ;; TODO: what comparison is used on the values? any dynamic dispatch there?
      [($ SwitchStatement loc expr (($ CaseClause _ qs as) ...))
       (with-syntax ([e (compile-expression expr)]
                     [(x v break falling-through?) (generate-temporaries '(x v break falling-through?))])
         (with-syntax ([(q ...) (map (lambda (q)
                                       (if q
                                           (with-syntax ([test-e (compile-expression q)])
                                             #'(lambda (x)
                                                 (equal? x test-e)))
                                           #'(lambda (x) #t)))
                                     qs)])
           (parameterize ([current-labels (cons (list #f #'break) (current-labels))])
             (with-syntax ([((a ...) ...) (map (lambda (stmts)
                                                 (map compile-statement stmts))
                                               as)])
               (syntax/loc* loc
                 (let ([v e])
                   (let/ec break
                     (let ([falling-through? #f])
                       (when (or falling-through? (q v))
                         (set! falling-through? #t)
                         a ...)
                       ...
                       (current-completion)))))))))]
      ;; TODO: test LabelledStatement
      [($ LabelledStatement loc label (and loop (? loop?)))
       (let ([label-name (Identifier-name label)])
         (with-syntax ([(break continue) (generate-temporaries '(break continue))])
           (parameterize ([current-labels (cons (list label-name #'break #'continue)
                                                (current-labels))])
             (compile-loop loop #'break #'continue))))]
      [($ LabelledStatement loc label statement)
       (let ([label-name (Identifier-name label)])
         (with-syntax ([(break) (generate-temporaries '(break))])
           (parameterize ([current-labels (cons (list label-name #'break)
                                                (current-labels))])
             (with-syntax ([s (compile-statement statement)])
               (syntax/loc* loc
                 (let/ec break s))))))]
      ;; TODO: test ThrowStatement
      [($ ThrowStatement loc value)
       (with-syntax ([stxloc (region->syntax loc)]
                     [e (compile-expression value)])
         (syntax/loc* loc
           (raise-runtime-exception stxloc e)))]
      ;; TODO: test TryStatement
      ;; TODO: add conditions to CatchClause ast and compile that too
      ;; TODO: need an error for try with no catch or finally
      ;; TODO: handle try/finally with return in finally
      [($ TryStatement loc body catches finally)
       (with-syntax ([body-s (compile-statement body)]
                     [(catch-e ...) (map compile-catch-clause catches)])
         (with-syntax ([try-catch #'(with-handlers ([exn:fail:javascript:runtime? catch-e]
                                                    ...)
                                       body-s)])
           (if finally
               (with-syntax ([finally-s (compile-statement finally)])
                 (syntax/loc* loc
                   (begin (dynamic-wind
                            void
                            (lambda () try-catch)
                            (lambda () finally-s))
                          (current-completion))))
               (syntax/loc* loc try-catch))))]
      ))

  (define (compile-catch-clause clause)
    (match clause
      [($ CatchClause loc exn catch)
       (with-syntax ([(exn-value) (generate-temporaries '(exn-value))])
         (let-values ([(var-stx-ids add-var bind-var) (make-bindings (list exn)
                                                                     (Term-location exn)
                                                                     (list
                                                                      (syntax/loc (region->syntax loc)
                                                                        (exn:fail:javascript:runtime-value exn-value))))])
           (with-syntax ([body (add-var (parameterize ([static-environment (bind-var (static-environment))])
                                          (compile-statement catch)))])
             (syntax/loc (region->syntax loc)
               (lambda (exn-value) body)))))]))

  ;; TODO: test loops
  (define (compile-loop stmt break-id continue-id)
    (match stmt
      [($ DoWhileStatement loc body test)
       (with-syntax ([body-s (compile-statement body)]
                     [test-e (compile-expression test)]
                     [break break-id]
                     [continue continue-id])
         (syntax/loc* loc
           (let/ec break
             (let loop ()
               (let/ec continue body-s)
               (if (true-value? test-e)
                   (loop)
                   (current-completion))))))]
      [($ WhileStatement loc test body)
       (with-syntax ([test-e (compile-expression test)]
                     [body-s (compile-statement body)]
                     [break break-id]
                     [continue continue-id])
         (syntax/loc* loc
           (let/ec break
             (let loop ()
               (if (true-value? test-e)
                   (begin (let/ec continue body-s)
                          (loop))
                   (current-completion))))))]
      [($ ForStatement loc init test incr body)
       (with-syntax ([init-e (if init
                                 (compile-expression init)
                                 #'(void))]
                     [test-e (if test
                                 (compile-expression test)
                                 #'(quote true))]
                     [incr-e (if incr
                                 (compile-expression incr)
                                 #'(void))]
                     [body-s (compile-statement body)]
                     [break break-id]
                     [continue continue-id]
                     [(loop) (generate-temporaries '(loop))])
         (syntax/loc* loc
           (begin
             init-e
             (let/ec break
               (let loop ()
                 (if (true-value? test-e)
                     (begin (let/ec continue body-s)
                            incr-e
                            (loop))
                     (current-completion)))))))]
      [($ ForInStatement loc lhs container body)
       (with-syntax ([(object next-key key) (generate-temporaries '(object next-key key))])
         (with-syntax ([container-e (compile-expression container)]
                       [update (compile-assignment lhs #'key)]
                       [body-s (compile-statement body)]
                       [break break-id]
                       [continue continue-id])
           (syntax/loc (region->syntax loc)
             (let/ec break
               (let* ([object container-e]
                      [next-key (object-keys-stream object)])
                 (let loop ()
                   (let ([key (next-key)])
                     (if key
                         (begin
                           update
                           (let/ec continue body-s)
                           (loop))
                         (current-completion)))))))))]
      ))

  (define (field-reference? x)
    (or (BracketReference? x)
        (DotReference? x)))

  ;; TODO: test cases for all expression forms
  (define (compile-expression expr)
    (match expr
      [($ StringLiteral loc value)
       (build-syntax value loc)]
      [($ NumericLiteral loc value)
       (build-syntax value loc)]
      [($ BooleanLiteral loc value)
       (if value
           (syntax/loc* loc 'true)
           (syntax/loc* loc 'false))]
      [($ NullLiteral loc)
       (syntax/loc* loc '())]
      [($ RegexpLiteral loc pattern global? case-insensitive?)
       (begin (printf "expression not compiled: ~v~n" expr)
              #'"<<regular expression>>")]
      [($ ArrayLiteral loc elts)
       (with-syntax ([(e ...) (map compile-expression elts)])
         (syntax/loc* loc
           (build-array (evector e ...))))]
      [($ ObjectLiteral loc properties)
       (let ([names (map (lambda (prop)
                           (let ([name (car prop)])
                             (cond
                               [(NumericLiteral? name) (NumericLiteral-value name)]
                               [(StringLiteral? name) (StringLiteral-value name)]
                               [(Identifier? name) (Identifier->key name)])))
                         properties)]
             [values (map cdr properties)])
         (with-syntax ([(key ...) names]
                       [(e ...) (map compile-expression values)])
           (syntax/loc* loc
             (build-object
              (object-table [key e] ...)))))]
      [($ ThisReference loc)
       (syntax/loc (region->syntax loc)
         (current-this))]
      [(? reference-expression?)
       (compile-lookup expr)]
      [($ NewExpression loc constructor args)
       (with-syntax ([stxloc (region->syntax loc)]
                     [constructor-e (compile-expression constructor)]
                     [(e ...) (map compile-expression args)]
                     [(ctor) (generate-temporaries '(ctor))])
         (syntax/loc* loc
           (let ([ctor constructor-e])
             (unless (object? ctor)
               (raise-runtime-type-error stxloc "constructor" ctor))
             ((object-construct ctor) (evector e ...)))))]
      [($ PostfixExpression loc operand op)
       (with-syntax ([op-e (if (eq? op '++) #'js:+ #'js:-)]
                     [operand-e (compile-expression operand)]
                     [update (compile-expression (make-AssignmentExpression loc
                                                                            operand
                                                                            (if (eq? op '++) '+= '-=)
                                                                            (make-NumericLiteral loc 1)))]
                     [(v) (generate-temporaries '(v))])
         (syntax/loc (region->syntax loc)
           (let ([v (value->number operand-e)])
             update
             v)))]
      [($ PrefixExpression loc op operand)
       (cond
         [(memq op '(++ --))
          (let ([op (if (eq? op '++) '+= '-=)])
            (compile-expression
             (make-AssignmentExpression loc operand op (make-NumericLiteral loc 1))))]
         [(eq? op 'delete)
          (compile-deletion operand)]
         [else
          (with-syntax ([op-e (operator->syntax op)]
                        [operand-e (compile-expression operand)])
            (syntax/loc* loc
              (op-e operand-e)))])]
      [($ InfixExpression loc left '&& right)
       (with-syntax ([left-e (compile-expression left)]
                     [right-e (compile-expression right)])
         (syntax/loc* loc
           (if (true-value? left-e) right-e 'false)))]
      [($ InfixExpression loc left '\|\| right)
       (with-syntax ([left-e (compile-expression left)]
                     [right-e (compile-expression right)]
                     [(tmp) (generate-temporaries '(tmp))])
         (syntax/loc* loc
           (let ([tmp left-e])
             (if (true-value? tmp) tmp right-e))))]
      [($ InfixExpression loc left op right)
       (with-syntax ([left-e (compile-expression left)]
                     [op-e (operator->syntax op)]
                     [right-e (compile-expression right)])
         (syntax/loc* loc
           (op-e left-e right-e)))]
      [($ ConditionalExpression loc test consequent alternate)
       (with-syntax ([test-e (compile-expression test)]
                     [consequent-e (compile-expression consequent)]
                     [alternate-e (compile-expression alternate)])
         (syntax/loc* loc
           (if test-e consequent-e alternate-e)))]
      [($ AssignmentExpression loc left '= right)
       (compile-assignment left (compile-expression right))]
      [($ AssignmentExpression loc left op right)
       (compile-expression
        (make-AssignmentExpression loc
                                   left
                                   '=
                                   (make-InfixExpression (Term-location right)
                                                         left
                                                         (assignment-operator->infix-operator op)
                                                         right)))]
      [($ FunctionExpression/hoisted loc name args body funs vars)
       (compile-function loc name args body funs vars)]
      [($ LetExpression loc bindings body)
       (let-values ([(var-stx-ids add-vars bind-vars) (make-bindings (map VariableInitializer-id bindings)
                                                                     loc
                                                                     (map (compose compile-expression VariableInitializer-init) bindings))])
         (with-syntax ([body (parameterize ([static-environment (bind-vars (static-environment))])
                               (compile-expression body))])
           (add-vars (syntax/loc (region->syntax loc) body))))]
      ;; TODO: inherit environment if definitely a call to `eval' (10.2.2)
      [($ CallExpression loc (and method ($ BracketReference _ container key)) args)
       (with-syntax ([stxloc (region->syntax loc)]
                     [container-e (compile-expression container)]
                     [key-e (compile-expression key)]
                     [(container-val key-val function-val) (generate-temporaries '(container-val key-val function-val))]
                     [(arg-e ...) (map compile-expression args)]
                     [(arg-val ...) (generate-temporaries args)])
         (syntax/loc (region->syntax loc)
           (let* ([container-val container-e]
                  [key-val key-e]
                  [function-val (object-get container-val key-val)]
                  [arg-val arg-e] ...)
             (parameterize ([current-this container-val])
               ;; TODO: what if there is no method?!
               (call function-val
                     (evector arg-val ...)
                     (lambda (str1 str2)
                       (raise-runtime-type-error stxloc str1 str2)))))))]
      [($ CallExpression loc ($ DotReference _ container id) args)
       (with-syntax ([stxloc (region->syntax loc)]
                     [container-e (compile-expression container)]
                     [key-val (Identifier->key id)]
                     [(container-val function-val) (generate-temporaries '(container-val function-val))]
                     [(arg-e ...) (map compile-expression args)]
                     [(arg-val ...) (generate-temporaries args)])
         (syntax/loc (region->syntax loc)
           (let* ([container-val container-e]
                  [function-val (object-get container-val key-val)]
                  [arg-val arg-e] ...)
             (parameterize ([current-this container-val])
               ;; TODO: what if there is no method?!
               (call function-val
                     (evector arg-val ...)
                     (lambda (str1 str2)
                       (raise-runtime-type-error stxloc str1 str2)))))))]
      [($ CallExpression loc function args)
       (with-syntax ([stxloc (region->syntax loc)]
                     [function-e (compile-expression function)]
                     [(arg-e ...) (map compile-expression args)]
                     [(function-val arg-val ...) (generate-temporaries (cons 'function-val args))])
         (syntax/loc (region->syntax loc)
           (let ([function-val function-e]
                 [arg-val arg-e] ...)
             (parameterize ([current-this global-object])
               (call function-val
                     (evector arg-val ...)
                     (lambda (str1 str2)
                       (raise-runtime-type-error stxloc str1 str2)))))))]
      [($ ParenExpression loc expr)
       (compile-expression expr)]
      [($ ListExpression loc ())
       #'(void)]
      [($ ListExpression loc exprs)
       (with-syntax ([(e ...) (map compile-expression exprs)])
         (syntax/loc* loc
           (begin e ...)))]
      ))

  (provide compile-script compile-interaction compile-function-expression with-syntax-errors))
