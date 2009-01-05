(module hoist mzscheme
  (require (planet "contract-utils.ss" ("cobbe" "contract-utils.plt" 1 0))
           (all-except (file "../../list.plt-2.0-hacked/list.ss") any)
           (lib "contract.ss")
           (lib "match.ss")
           (lib "struct.ss")
           "../syntax/ast.ss"
           "../syntax/token.ss")

  (define-struct (FunctionDeclaration/hoisted FunctionDeclaration) (functions variables))
  (define-struct (FunctionExpression/hoisted FunctionExpression) (functions variables))
  (define-struct (BlockStatement/hoisted BlockStatement) (functions variables))

  ;; (listof FunctionDeclaration/hoisted) * (listof Identifier) * (listof FunctionDeclaration/hoisted) * (listof Identifier)
  (define-struct hoisting (functions vars let-functions let-vars) #f)

  ;; null-hoisting : hoisting
  (define null-hoisting (make-hoisting null null null null))

  ;; append-hoisting : hoisting * hoisting -> hoisting
  (define (append-hoisting . hs)
    (match hs
      [(($ hoisting funss varss let-funss let-varss) ...)
       (make-hoisting (apply append funss)
                      (apply append varss)
                      (apply append let-funss)
                      (apply append let-varss))]))

  ;; cons-function-hoisting : FunctionDeclaration/hoisted * hoisting -> hoisting
  (define (cons-function-hoisting fun h)
    (copy-struct hoisting h [hoisting-functions (cons fun (hoisting-functions h))]))

  ;; cons-var-hoisting : Identifier * hoisting -> hoisting
  (define (cons-var-hoisting var h)
    (copy-struct hoisting h [hoisting-vars (cons var (hoisting-vars h))]))

  ;; cons-let-function-hoisting : FunctionDeclaration/hoisted * hoisting -> hoisting
  (define (cons-let-function-hoisting fun h)
    (copy-struct hoisting h [hoisting-let-functions (cons fun (hoisting-let-functions h))]))

  ;; cons-let-var-hoisting : Identifier * hoisting -> hoisting
  (define (cons-let-var-hoisting var h)
    (copy-struct hoisting h [hoisting-let-vars (cons var (hoisting-let-vars h))]))

  ;; a (continuation a -> b) is:
  ;; 
  ;;   - (hoisting a -> b)

  (define (continuation/c c)
    (hoisting? c . -> . any))

  ;; an (a ->k b) is:
  ;;
  ;;   - (a (continuation b -> c) -> c)

  (define-syntax provide/contract/k
    (syntax-rules (->k)
      [(_ [name (domain . ->k . range)] ...)
       (provide/contract
         [name (domain (continuation/c range) . -> . any)]
         ...)]))

  ;; unique-vars : (listof FunctionDeclaration/hoisted) * (listof Identifier) -> (listof Identifier)
  (define (unique-vars funs vars)
    (lset-difference Identifier=?
      (delete-duplicates vars Identifier=?)
      (map FunctionDeclaration-name funs)))

  ;; ===========================================================================
  ;; TOP-LEVEL HOISTING FUNCTIONS
  ;; ===========================================================================

  ;; wrap-in-implicit-block : region * (listof Statement) * (listof FunctionDeclaration/hoisted) * (listof Identifier) -> (listof Statement)
  (define (wrap-in-implicit-block loc stmts let-funs let-vars)
    (list (make-BlockStatement/hoisted loc stmts let-funs let-vars)))

  ;; hoist-function-declaration : FunctionDeclaration -> FunctionDeclaration/hoisted
  (define (hoist-function-declaration decl)
    (match decl
      [($ FunctionDeclaration location name args body)
       (hoist-source-elements body
         (lambda (h stmts)
           (match h
             [($ hoisting funs vars let-funs let-vars)
              (make-FunctionDeclaration/hoisted location
                                                name
                                                args
                                                (wrap-in-implicit-block location stmts let-funs let-vars)
                                                funs
                                                (unique-vars funs vars))])))]))

  ;; hoist-script : (listof SourceElement) -> (listof FunctionDeclaration/hoisted)
  ;;                                          (listof Identifier)
  ;;                                          (listof Statement)
  (define (hoist-script elts)
    (hoist-source-elements elts
      (lambda (h stmts)
        (match h
          [($ hoisting funs vars let-funs let-vars)
           (values funs
                   (unique-vars funs vars)
                   (wrap-in-implicit-block (@ (first elts) (last elts)) stmts let-funs let-vars))]))))

  ;; ===========================================================================
  ;; COMPOUND HOISTING FUNCTIONS
  ;; ===========================================================================

  ;; map-k : (a ->k b) (listof a) ->k (listof b)
  (define (map-k hoist1 elts k)
    (if (null? elts)
        (k null-hoisting null)
        (hoist1 (car elts)
          (lambda (hoisting1 result)
            (map-k hoist1 (cdr elts)
              (lambda (hoisting2 results)
                (k (append-hoisting hoisting1 hoisting2)
                   (cons result results))))))))

  ;; filter-map-k : (a ->k (optional b)) (listof a) ->k (listof b)
  (define (filter-map-k hoist1 elts k)
    (if (null? elts)
        (k null-hoisting null)
        (hoist1 (car elts)
          (lambda (hoisting1 result)
            (filter-map-k hoist1 (cdr elts)
              (lambda (hoisting2 results)
                (k (append-hoisting hoisting1 hoisting2)
                   (if result (cons result results) results))))))))

  ;; append-map-k : (a ->k (listof b)) (listof a) ->k (listof b)
  (define (append-map-k hoist1 elts k)
    (if (null? elts)
        (k null-hoisting null)
        (hoist1 (car elts)
          (lambda (hoisting1 results1)
            (append-map-k hoist1 (cdr elts)
              (lambda (hoisting2 results2)
                (k (append-hoisting hoisting1 hoisting2)
                   (append results1 results2))))))))

  ;; hoist-source-elements : (listof SourceElement) ->k (listof Statement)
  (define (hoist-source-elements elts k)
    (map-k hoist-source-element elts k))

  ;; hoist-optional-expression : (optional Expression) -> (optional Expression)
  (define (hoist-optional-expression expr)
    (and expr (hoist-expression expr)))

  ;; hoist-substatements : (listof SourceElement) ->k (listof Statement)
  (define (hoist-substatements stmts k)
    (hoist-source-elements stmts k))
    ;(filter-map-k hoist-substatement stmts k))

  ;; hoist-var-initializers : (listof VariableInitializer) (optional region) ->k Expression
  (define (hoist-var-initializers decls loc k)
    (filter-map-k hoist-var-initializer decls
      (lambda (h exprs)
        (k h (if (and (pair? exprs)
                      (null? (cdr exprs)))
                 (car exprs)
                 (make-ListExpression loc exprs))))))

  ;; hoist-let-var-initializers : (listof VariableInitializer) (optional region) ->k Expression
  (define (hoist-let-var-initializers decls loc k)
    (filter-map-k hoist-let-var-initializer decls
      (lambda (h exprs)
        (k h (if (and (pair? exprs)
                      (null? (cdr exprs)))
                 (car exprs)
                 (make-ListExpression loc exprs))))))

  ;; hoist-case-clauses : (listof CaseClause) ->k (listof CaseClause)
  (define (hoist-case-clauses cases k)
    (map-k hoist-case-clause cases k))

  ;; hoist-catch-clauses : (listof CatchClause) ->k (listof CatchClause)
  (define (hoist-catch-clauses catches k)
    (map-k hoist-catch-clause catches k))

  ;; ===========================================================================
  ;; CORE HOISTING FUNCTIONS
  ;; ===========================================================================

  ;; hoist-source-element : SourceElement ->k Statement
  (define (hoist-source-element src k)
    (if (Declaration? src)
        (hoist-declaration src k)
        (hoist-statement src k)))

  ;; hoist-declaration : Declaration ->k Statement
  (define (hoist-declaration decl0 k)
    (cond
      [(FunctionDeclaration? decl0)
       (k (cons-function-hoisting (hoist-function-declaration decl0) null-hoisting)
          (make-EmptyStatement (Term-location decl0)))]
      [(VariableDeclaration? decl0)
       (hoist-variable-declaration decl0
         (lambda (h expr)
           (k h (make-ExpressionStatement (Term-location decl0) expr))))]
      [(LetDeclaration? decl0)
       (hoist-let-declaration decl0
         (lambda (h expr)
           (k h (make-ExpressionStatement (Term-location decl0) expr))))]
      [else (error 'hoist-declaration (format "unrecognized declaration: ~v" decl0))]))

  ;; hoist-variable-declaration : VariableDeclaration ->k Expression
  (define (hoist-variable-declaration decl0 k)
    (match decl0
      [($ VariableDeclaration loc bindings)
       (hoist-var-initializers bindings loc k)]))

  ;; hoist-let-declaration : LetDeclaration ->k Expression
  (define (hoist-let-declaration decl0 k)
    (match decl0
      [($ LetDeclaration loc bindings)
       (hoist-let-var-initializers bindings loc k)]))

  ;; hoist-substatement : SourceElement ->k (optional Statement)
  (define (hoist-substatement src0 k)
    (hoist-source-element src0 k))

;    (if (FunctionDeclaration? src0)
;        (k (cons-function-hoisting (hoist-function-declaration src0) null-hoisting) #f)
;        (hoist-statement src0 k)))

  ;; hoist-var-initializer : VariableInitializer ->k (optional Expression)
  (define (hoist-var-initializer decl k)
    (match decl
      [($ VariableInitializer loc id #f)
       (k (cons-var-hoisting id null-hoisting) #f)]
      [($ VariableInitializer loc id init)
       (k (cons-var-hoisting id null-hoisting)
          (make-AssignmentExpression loc (make-VarReference (Term-location id) id) '= (hoist-expression init)))]))

  ;; hoist-let-var-initializer : VariableInitializer ->k (optional Expression)
  (define (hoist-let-var-initializer decl k)
    (match decl
      [($ VariableInitializer loc id #f)
       (k (cons-let-var-hoisting id null-hoisting) #f)]
      [($ VariableInitializer loc id init)
       (k (cons-let-var-hoisting id null-hoisting)
          (make-AssignmentExpression loc (make-VarReference (Term-location id) id) '= (hoist-expression init)))]))

  ;; hoist-case-clause : CaseClause ->k CaseClause
  (define (hoist-case-clause case k)
    (match case
      [($ CaseClause loc #f answer)
       (hoist-substatements answer
         (lambda (h answer)
           (k h (make-CaseClause loc #f answer))))]
      [($ CaseClause loc question answer)
       (hoist-substatements answer
         (lambda (h answer)
           (k h (make-CaseClause loc (hoist-expression question) answer))))]))

  ;; hoist-catch-clause : CatchClause ->k CatchClause
  (define (hoist-catch-clause catch k)
    (match catch
      [($ CatchClause loc id body0)
       (hoist-statement body0
         (lambda (h body)
           (k h (make-CatchClause loc id body))))]))

  (define (to-location x)
    (cond
      [(not x) #f]
      [(position? x) x]
      [else (ast-location x)]))

  ;; optional-statement->statement : (optional Statement) (optional has-location) -> Statement
  (define (optional-statement->statement stmt loc)
    (or stmt (make-EmptyStatement (to-location loc))))

  ;; hoist-statement : Statement ->k Statement
  (define (hoist-statement stmt k)
    (match stmt
      [($ BlockStatement loc stmts)
       (hoist-substatements stmts
         (lambda (h stmts)
           (match h
             [($ hoisting funs vars let-funs let-vars)
              (k (make-hoisting funs vars null null)
                 (make-BlockStatement/hoisted loc stmts let-funs let-vars))])))]
      [($ ExpressionStatement loc expr)
       (k null-hoisting (make-ExpressionStatement loc (hoist-expression expr)))]
      [($ IfStatement loc test consequent0 alternate0)
       (hoist-substatement consequent0
         (lambda (h1 consequent)
           (if alternate0
               (hoist-substatement alternate0
                 (lambda (h2 alternate)
                   (k (append-hoisting h1 h2)
                      (make-IfStatement loc
                                        (hoist-expression test)
                                        (optional-statement->statement consequent consequent0)
                                        alternate))))
               (k h1 (make-IfStatement loc (hoist-expression test) (optional-statement->statement consequent consequent0) #f)))))]
      [($ DoWhileStatement loc body0 test)
       (hoist-substatement body0
         (lambda (h body)
           (k h (make-DoWhileStatement loc (optional-statement->statement body body0) (hoist-expression test)))))]
      [($ WhileStatement loc test body0)
       (hoist-substatement body0
         (lambda (h body)
           (k h (make-WhileStatement loc (hoist-expression test) (optional-statement->statement body body0)))))]
      [($ ForStatement loc init test incr body0)
       (if (VariableDeclaration? init)
           (hoist-variable-declaration init
             (lambda (h1 init)
               (hoist-substatement body0
                 (lambda (h2 body)
                   (k (append-hoisting h1 h2)
                      (make-ForStatement loc
                                         init
                                         (hoist-optional-expression test)
                                         (hoist-optional-expression incr)
                                         (optional-statement->statement body body0)))))))
           (hoist-substatement body0
             (lambda (h body)
               (k h (make-ForStatement loc
                                       (hoist-optional-expression init)
                                       (hoist-optional-expression test)
                                       (hoist-optional-expression incr)
                                       (optional-statement->statement body body0))))))]
      [($ ForInStatement loc (and lhs (? Expression?)) container body0)
       (hoist-substatement body0
         (lambda (h body)
           (k h (make-ForInStatement loc
                                     (hoist-expression lhs)
                                     (hoist-expression container)
                                     (optional-statement->statement body body0)))))]
      [($ ForInStatement loc ($ VariableDeclaration _ (($ VariableInitializer v-loc id #f))) container body0)
       (hoist-substatement body0
         (lambda (h body)
           (k (cons-var-hoisting id h)
              (make-ForInStatement loc
                                   (make-VarReference v-loc id)
                                   (hoist-expression container)
                                   (optional-statement->statement body body0)))))]
      [($ ReturnStatement loc expr)
       (k null-hoisting (make-ReturnStatement loc (hoist-optional-expression expr)))]
      [($ LetStatement loc (($ VariableInitializer binding-locs binding-names binding-inits) ...) body0)
       (hoist-substatement body0
         (lambda (h body)
           (k h (make-LetStatement loc
                                   (map make-VariableInitializer binding-locs binding-names (map hoist-optional-expression binding-inits))
                                   (optional-statement->statement body body0)))))]
      [($ WithStatement loc test body0)
       (hoist-substatement body0
         (lambda (h body)
           (k h (make-WithStatement loc (hoist-expression test) (optional-statement->statement body body0)))))]
      [($ SwitchStatement loc expr cases)
       (hoist-case-clauses cases
         (lambda (h cases)
           (k h (make-SwitchStatement loc (hoist-expression expr) cases))))]
      [($ LabelledStatement loc label stmt0)
       (hoist-substatement stmt0
         (lambda (h stmt)
           (k h (make-LabelledStatement loc label (optional-statement->statement stmt stmt0)))))]
      [($ ThrowStatement loc expr)
       (k null-hoisting (make-ThrowStatement loc (hoist-expression expr)))]
      [($ TryStatement loc body0 catch0 finally0)
       (hoist-statement body0
         (lambda (h1 body)
           (hoist-catch-clauses catch0
             (lambda (h2 catch)
               (if finally0
                   (hoist-statement finally0
                     (lambda (h3 finally)
                       (k (append-hoisting h1 h2 h3)
                          (make-TryStatement loc body catch finally))))
                   (k (append-hoisting h1 h2)
                      (make-TryStatement loc body catch #f)))))))]
      [_ (k null-hoisting stmt)]))

  ;; hoist-expression : Expression -> Expression
  (define (hoist-expression expr)
    (match expr
      [($ ArrayLiteral loc elts)
       (make-ArrayLiteral loc (map hoist-optional-expression elts))]
      [($ ObjectLiteral loc ([props . vals] ...))
       (make-ObjectLiteral loc (map cons props (map hoist-expression vals)))]
      [($ BracketReference loc container key)
       (make-BracketReference loc (hoist-expression container) (hoist-expression key))]
      [($ DotReference loc container id)
       (make-DotReference loc (hoist-expression container) id)]
      [($ NewExpression loc ctor args)
       (make-NewExpression loc (hoist-expression ctor) (map hoist-expression args))]
      [($ PostfixExpression loc expr op)
       (make-PostfixExpression loc (hoist-expression expr) op)]
      [($ PrefixExpression loc op expr)
       (make-PrefixExpression loc op (hoist-expression expr))]
      [($ InfixExpression loc left op right)
       (make-InfixExpression loc (hoist-expression left) op (hoist-expression right))]
      [($ ConditionalExpression loc test consequent alternate)
       (make-ConditionalExpression loc
                                   (hoist-expression test)
                                   (hoist-expression consequent)
                                   (hoist-expression alternate))]
      [($ AssignmentExpression loc left op right)
       (make-AssignmentExpression loc
                                  (hoist-expression left)
                                  op
                                  (hoist-expression right))]
      [($ FunctionExpression loc name args body)
       (hoist-source-elements body
         (lambda (h body)
           (match h
             [($ hoisting funs vars let-funs let-vars)
              (make-FunctionExpression/hoisted loc
                                               name
                                               args
                                               ;; TODO: should FunctionExpressions contain a block instead of a list?
                                               (wrap-in-implicit-block loc body let-funs let-vars)
                                               funs
                                               (unique-vars funs vars))])))]
      [($ LetExpression loc (($ VariableInitializer binding-locs binding-names binding-inits) ...) body)
       (make-LetExpression loc
                           (map make-VariableInitializer binding-locs binding-names (map hoist-optional-expression binding-inits))
                           (hoist-expression body))]
      [($ CallExpression loc method args)
       (make-CallExpression loc (hoist-expression method) (map hoist-expression args))]
      [($ ParenExpression loc expr)
       (make-ParenExpression loc (hoist-expression expr))]
      [($ ListExpression loc exprs)
       (make-ListExpression loc (map hoist-expression exprs))]
      [_ expr]))

  (provide/contract
    [continuation/c ((union flat-contract? predicate/c) . -> . contract?)])

  (provide/contract/k
    [hoist-source-elements ((listof SourceElement?) . ->k . (listof Statement?))]
    [hoist-declaration (Declaration? . ->k . Statement?)]
    [hoist-variable-declaration (VariableDeclaration? . ->k . Expression?)]
    [hoist-let-declaration (LetDeclaration? . ->k . Expression?)]
    [hoist-source-element (SourceElement? . ->k . Statement?)]
    [hoist-statement (Statement? . ->k . Statement?)]
    [hoist-substatement (SubStatement? . ->k . (optional/c Statement?))]
    [hoist-substatements ((listof SubStatement?) . ->k . (listof Statement?))]
    [hoist-var-initializer (VariableInitializer? . ->k . (optional/c Expression?))]
    ;[hoist-var-initializers ((listof VariableInitializer?) (optional/c region?) . ->k . Expression?)]
    [hoist-case-clause (CaseClause? . ->k . CaseClause?)]
    [hoist-case-clauses ((listof CaseClause?) . ->k . (listof CaseClause?))]
    [hoist-catch-clause (CatchClause? . ->k . CatchClause?)]
    [hoist-catch-clauses ((listof CatchClause?) . ->k . (listof CatchClause?))])

  (provide/contract
    [hoist-expression (Expression? . -> . Expression?)]
    ;[hoist-expressions ((listof Expression?) . -> . (listof Expression?))]
    [hoist-function-declaration (FunctionDeclaration? . -> . FunctionDeclaration/hoisted?)]
    [hoist-script ((listof SourceElement?) . -> . (values (listof FunctionDeclaration/hoisted?)
                                                          (listof Identifier?)
                                                          (listof Statement?)))])

  (provide/contract
    (struct (FunctionDeclaration/hoisted FunctionDeclaration) ([location (optional/c region?)]
                                                               [name Identifier?]
                                                               [args (listof Identifier?)]
                                                               [body (listof Statement?)]
                                                               [functions (listof FunctionDeclaration/hoisted?)]
                                                               [variables (listof Identifier?)]))
    (struct (FunctionExpression/hoisted FunctionExpression) ([location (optional/c region?)]
                                                             [name (optional/c Identifier?)]
                                                             [args (listof Identifier?)]
                                                             [body (listof Statement?)]
                                                             [functions (listof FunctionDeclaration/hoisted?)]
                                                             [variables (listof Identifier?)]))
    (struct (BlockStatement/hoisted BlockStatement) ([location (optional/c region?)]
                                                     [statements (listof Statement?)]
                                                     [functions (listof FunctionDeclaration/hoisted?)]
                                                     [variables (listof Identifier?)]))
    ))
