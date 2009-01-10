(module parse mzscheme
  (require (planet "contract-utils.ss" ("cobbe" "contract-utils.plt" 1 0))
           (all-except (lib "list.ss" "srfi" "1") any)
           (lib "contract.ss")
           (lib "match.ss")
           (lib "class.ss")
           (lib "etc.ss")
           "ast.ss"
           "../config.ss"
           "tokenize.ss"
           "token.ss")

  ;; TODO:
  ;;   - disallow newlines before postfix operators (7.9.1)
  ;;   - Mozilla extensions:
  ;;      * catch guards
  ;;      * multiple catch clauses
  ;;      * const
  ;;   - let function
  ;;   - destructuring assignment

  ;; ===========================================================================
  ;; INHERITED ATTRIBUTES
  ;; ===========================================================================

  ;; Is the binary `in' operator disabled? (i.e., within `for' initialization)
  (define disable-in? (make-parameter #f))

  ;; Is the binary `,' operator disabled? (i.e., within argument lists)
  (define disable-comma? (make-parameter #f))

  ;; ===========================================================================
  ;; OPERATORS AND PRECEDENCE
  ;; ===========================================================================

  (define operatorTypePrecedenceList
    '((\;)
      (\,)
      (ASSIGN)
      (? :)
      (\|\|)
      (&&)
      (\|)
      (^)
      (&)
      (== != === !==)
      (< <= >= > in instanceof)
      (<< >> >>>)
      (+ -)
      (* / %)
      (delete void typeof ! ~ UNARY)
      (++ --)
      (new)))

  (define operatorTypePrecedence
    (fold (lambda (ops index result)
            (append (map (lambda (op)
                           (cons op index))
                         ops)
                    result))
          null
          operatorTypePrecedenceList
          (iota (length operatorTypePrecedenceList))))

  ;; operator-precedence : token -> nat
  (define (operator-precedence op)
    (cdr (assq (token-type op) operatorTypePrecedence)))

  ;; infix-operator-token? : token -> boolean
  (define (infix-operator-token? token)
    (let ([tt (token-type token)])
      (and (not (and (disable-in?) (eq? tt 'in)))
           (or (eq? tt 'ASSIGN)
               (eq? tt '?)
               (and (eq? tt '\,) (not (disable-comma?)))
               (infix-operator? tt)))))

  ;; postfix-operator-token? : token -> boolean
  (define (postfix-operator-token? token)
    (postfix-operator? (token-type token)))

  ;; binds-tighter? : token token -> boolean
  (define (binds-tighter? op1 op2)
    (> (operator-precedence op1)
       (operator-precedence op2)))

  ;; ===========================================================================
  ;; PARSING UTILITIES
  ;; ===========================================================================

  (define abstract-parser%
    (class object%
      (init tokenizer)
      (define t tokenizer)

      ;; while : symbol ... -> condition
      (define/public (while . delimiters)
        (lambda (first?)
          (let ([token (send t peek-token)])
            (and (memq (token-type token) delimiters)
                 token))))

      ;; until : symbol ... -> condition
      (define/public (until . delimiters)
        (lambda (first?)
          (let ([token (send t peek-token)])
            (and (not (memq (token-type token) delimiters))
                 token))))

      ;; sep : symbol condition -> condition
      (define/public (sep sym condition)
        (lambda (first?)
          (cond
            [first? (condition #t)]
            [(eq? (token-type (send t peek-token)) sym)
             (let ([token (send t read-token)])
               (or (condition #f)
                   (begin (send t unread-token) #f)))]
            [else #f])))

      ;; map-tokens : condition (token -> a) -> (listof a)
      (define/public (map-tokens condition proc)
        (if (not (condition #t))
            null
            (let loop ([acc (list (proc))])
              (if (not (condition #f))
                  (reverse acc)
                  (loop (cons (proc) acc))))))

      (super-make-object)))

  ;; token->Identifier : token -> Identifier
  (define (token->Identifier token)
    (make-Identifier (token-location token) (token-contents token)))

  ;; ===========================================================================
  ;; PARSER
  ;; ===========================================================================

  (define parser<%>
    (interface ()
      parse-source-element
      parse-source-elements
      parse-expression
      parse-function-expression
      skip-empty-tokens
      ))

  (define parser%
    (class* abstract-parser% (parser<%>)
      (init tokenizer)
      (define t tokenizer)

      (inherit while until sep map-tokens)

      (define (expression-on-same-line?)
        (not (memq (token-type (send t peek-token/same-line))
                   '(END NEWLINE \; \}))))

      ;; skip-empty-tokens : -> any
      (define/public (skip-empty-tokens)
        (send t skip-whitespace)
        (let ([token (send t peek-token/same-line)])
          (when (eq? (token-type token) 'NEWLINE)
            (send t read-token/same-line)
            (skip-empty-tokens))))

      ;; fail : string any ... -> <never>
      (define (fail fmt . args)
        (send/apply t fail fmt args))

      ;; fail : region string any ... -> <never>
      (define (fail/loc loc text fmt . args)
        (send/apply t fail/loc loc text fmt args))

      ;; TODO: unit tests for REPL parsing

      ;; parse-source-element : -> SourceElement
      (define/public (parse-source-element)
        (case (token-type (send t peek-token))
          [(function)
           (parse-function-definition #f)]
          [(\{)
           (parse-block)]
          [(if)
           (send t must-match 'if)
           (let ([test (parse-paren-expression)]
                 [consequent (parse-sub-statement)]
                 [alternate (and (send t match 'else) (parse-sub-statement))])
             (make-IfStatement (@ test (or alternate consequent))
                               test consequent alternate))]
          [(switch)
           (let ([switch-token (send t must-match 'switch)]
                 [test (parse-paren-expression)])
             (send t must-match '\{)
             (let ([cases (map-tokens (until '\})
                            (lambda ()
                              (let* ([token (send t read-token)]
                                     [condition (case (token-type token)
                                                  [(default) #f]
                                                  [(case) (parse-list-expression)]
                                                  [else
                                                   (fail/loc (token-location token)
                                                             (token-contents token)
                                                             "expected case or default")])]
                                     [colon-token (send t must-match ':)]
                                     [action (map-tokens (until 'case 'default '\})
                                               (lambda ()
                                                 (parse-sub-statement)))])
                                (make-CaseClause (@ token (if (pair? action) (last action) colon-token))
                                                 condition action))))])
               (let ([close-curly (send t must-match '\})])
                 (make-SwitchStatement (@ switch-token close-curly)
                                       test cases))))]
          [(for)
           (let ([for-token (send t must-match 'for)])
             (send t must-match '\()
             ;; The `in' operator is disabled in init expressions since
             ;; it's ambiguous with `for..in' expressions.
             (let ([init (parameterize ([disable-in? #t])
                           (let ([next (send t peek-token)])
                             (and (not (eq? (token-type next) '\;))
                                  (if (eq? (token-type next) 'var)
                                      (let ([bindings (begin (send t read-token)
                                                             (parse-variable-bindings))])
                                        (make-VariableDeclaration (@ (first bindings) (last bindings))
                                                                  bindings))
                                      (parse-list-expression)))))])
               (if (and init (send t match 'in))
                   ;; ForInStatement
                   (let ([lhs (match init
                                [(? ListExpression?)
                                 (fail/loc (token-location for-token)
                                           "for..."
                                           "invalid for..in left-hand side")]
                                [(? Expression?) init]
                                [($ VariableDeclaration loc (($ VariableInitializer _ id #f))) init]
                                [($ VariableDeclaration loc (($ VariableInitializer _ _ _)))
                                 (fail/loc loc
                                           "for..."
                                           "invalid for..in left-hand side")]
                                [($ VariableDeclaration loc (i1 is ...))
                                 (fail/loc loc
                                           "for..."
                                           "invalid for..in left-hand side")]
                                [_ (fail/loc (token-location for-token)
                                             "for..."
                                             "invalid for..in left-hand side")])])
                     (let ([expression (parse-list-expression)])
                       (send t must-match '\))
                       (let ([body (parse-sub-statement)])
                         (make-ForInStatement (@ for-token body)
                                              lhs expression body))))
                   ;; ForStatement
                   (begin (send t must-match '\;)
                          (let ([test (and (not (eq? (token-type (send t peek-token)) '\;))
                                           (parse-list-expression))]
                                [semicolon-token (send t must-match '\;)]
                                [incr (and (not (eq? (token-type (send t peek-token)) '\)))
                                           (parse-list-expression))]
                                [close-paren (send t must-match '\))]
                                [body (parse-sub-statement)])
                            (make-ForStatement (@ for-token body)
                                               init test incr body))))))]
          [(while)
           (let ([while-token (send t must-match 'while)])
             (let ([test (parse-paren-expression)]
                   [body (parse-sub-statement)])
               (make-WhileStatement (@ while-token body)
                                    test body)))]
          [(do)
           (let ([do-token (send t must-match 'do)])
             (let ([body (parse-sub-statement)]
                   [test (begin (send t must-match 'while)
                                (parse-paren-expression))]
                   [semicolon-token (match-semicolon (infer-do-while-semicolon?))])
               (make-DoWhileStatement (@ do-token (or semicolon-token test))
                                      body test)))]
          [(break continue)
           (let* ([token (send t read-token)]
                  [constructor (if (eq? (token-type token) 'break)
                                   make-BreakStatement
                                   make-ContinueStatement)]
                  ;; 7.9.1
                  [label (and (expression-on-same-line?)
                              (token->Identifier (send t must-match 'ID)))])
             ;; 12.12
             (let ([semicolon-token (match-semicolon #t)])
               (constructor (@ token (or semicolon-token label token))
                            label)))]
          [(try)
           (let ([try-token (send t must-match 'try)]
                 [body (parse-block)]
                 [catches (map-tokens (while 'catch)
                            (lambda ()
                              (let ([catch-token (send t must-match 'catch)])
                                (send t must-match '\()
                                (let ([id (token->Identifier (send t must-match 'ID))])
                                  (send t must-match '\))
                                  (let ([body (parse-block)])
                                    (make-CatchClause (@ catch-token body)
                                                      id body))))))]
                 [finally (and (send t match 'finally)
                               (parse-block))])
             (unless (or (enable-extended-catch-statements?) (<= (length catches) 1))
               (fail/loc (Term-location (cadr catches)) "catch..." "catch without preceding try"))
             (make-TryStatement (@ try-token (or finally (and (pair? catches) (last catches)) body))
                                body catches finally))]
          [(catch finally)
           (let ([token (send t read-token)])
             (fail/loc (token-location token)
                       (token-contents token)
                       "~a without preceding try" (token-type token)))]
          [(throw)
           (let ([throw-token (send t must-match 'throw)])
             ;; 7.9.1
             (unless (expression-on-same-line?)
               (fail/loc (token-location (send t peek-token/same-line))
                         "throw..."
                         "expected expression on same line"))
             (let ([expr (parse-list-expression)]
                   [semicolon-token (match-semicolon #t)])
               (make-ThrowStatement (@ throw-token (or semicolon-token expr))
                                    expr)))]
          [(return)
           (let ([return-token (send t must-match 'return)]
                 ;; 7.9.1
                 [value (and (expression-on-same-line?) (parse-list-expression))]
                 [semicolon-token (match-semicolon #t)])
             (make-ReturnStatement (@ return-token (or semicolon-token value return-token))
                                   value))]
          [(with)
           (let ([with-token (send t must-match 'with)]
                 [object (parse-paren-expression)]
                 [body (parse-sub-statement)])
             (make-WithStatement (@ with-token body)
                                 object body))]
          [(var)
           (let ([var-token (send t must-match 'var)]
                 [variables (parse-variable-bindings)]
                 [semicolon-token (match-semicolon #t)])
             (make-VariableDeclaration (@ var-token (or semicolon-token (last variables)))
                                       variables))]
          [(let)
           (if (eq? (token-type (send t peek-token 1)) '\()
               (let ([let-expr-or-stmt (parse-let #f)])
                 (if (Expression? let-expr-or-stmt)
                     (make-ExpressionStatement (Term-location let-expr-or-stmt) let-expr-or-stmt)
                     let-expr-or-stmt))
;               (parse-let #f)
               (let ([new-operator (send t read-token)]
                     [bindings (parse-variable-bindings)]
                     [semicolon-token (match-semicolon #t)])
                 (make-LetDeclaration (@ new-operator (or semicolon-token (last bindings)))
                                      bindings)))]
          [(\;)
           (make-EmptyStatement (token-location (send t must-match '\;)))]
          [else
           (if (and (eq? (token-type (send t peek-token)) 'ID)
                    (eq? (token-type (send t peek-token/infix-operator 1)) ':))
               (let ([label (token->Identifier (send t read-token))])
                 (send t read-token/infix-operator)
                 (let ([body (parse-sub-statement)])
                   (make-LabelledStatement (@ label body) label body)))
                 ;; NOTE: can never be a FunctionExpression (12.4)
               (let ([expr (parse-list-expression)]
                     [semicolon-token (match-semicolon #t)])
                 (make-ExpressionStatement (@ expr (or semicolon-token expr))
                                           expr)))]))

      ;; parse-sub-statement : -> SubStatement
      (define (parse-sub-statement)
        (let ([element (parse-source-element)])
          (when (and (FunctionDeclaration? element)
                     (not (allow-nested-function-declarations?)))
            (fail/loc (Term-location element)
                      "function..."
                      "illegally nested function declaration"))
          element))

      ;; parse-sub-statements : -> (listof SubStatement)
      (define (parse-sub-statements)
        (map-tokens (until '\} 'END)
          (lambda ()
            (parse-sub-statement))))

      ;; match-semicolon : boolean -> (optional token)
      (define (match-semicolon insert-semicolon?)
        (if (not insert-semicolon?)
            (send t must-match '\;)
            (send t match '\;)))

      ;; parse-block : -> BlockStatement
      (define (parse-block)
        (let ([open-curly (send t must-match '\{)]
              [body (parse-sub-statements)]
              [close-curly (send t must-match '\})])
          (make-BlockStatement (@ open-curly close-curly)
                               body)))

      ;; parse-source-elements : -> (listof SourceElement)
      (define/public (parse-source-elements)
        (map-tokens (until '\} 'END)
          (lambda ()
            (parse-source-element))))

      ;; parse-variable-bindings : -> (nelistof VariableInitializer)
      (define (parse-variable-bindings)
        (let ([result (map-tokens (sep '\, (while 'ID))
                        (lambda ()
                          (let ([var (send t must-match 'ID)]
                                [init (let ([token (send t match 'ASSIGN)])
                                        (when (and token (not (eq? (token-contents token) '=)))
                                          (fail/loc (token-location token)
                                                    (token-contents token)
                                                    "invalid variable intitialization"))
                                        (and token (parse-single-expression)))])
                            (make-VariableInitializer (@ var (or init var))
                                                      (token->Identifier var)
                                                      init))))])
          (when (null? result)
            (let ([bad-token (send t peek-token)])
              (fail/loc (token-location bad-token)
                        (token-contents bad-token)
                        "expected variable name")))
          result))

      ;; parse-let-expression : -> LetExpression
      (define (parse-let-expression)
        (parse-let #t))

      ;; parse-let : boolean -> (union LetExpression LetStatement)
      (define (parse-let expression-only?)
        (let ([new-operator (send t read-token)]
              [bindings (begin (send t match '\()
                               (begin0 (map-tokens (sep '\, (until '\)))
                                                   (lambda ()
                                                     (let ([id (send t must-match 'ID)]
                                                           [binding (cond
                                                                      [(send t match 'ASSIGN)
                                                                       => (lambda (tok)
                                                                            (and (eq? (token-contents tok) '=)
                                                                                 (parse-single-expression)))]
                                                                      [else #f])])
                                                       (make-VariableInitializer (@ id (or binding id))
                                                                                 (token->Identifier id)
                                                                                 binding))))
                                       (send t must-match '\))))])
          (let ([next-token (send t peek-token)])
            (if (eq? (token-type next-token) '\{)
                (begin
                  (when expression-only?
                    (fail/loc (token-location next-token)
                              (token-contents next-token)
                              "expected expression"))
                  (let ([body (parse-block)])
                    (make-LetStatement (@ new-operator body) bindings body)))
                (let ([body (parse-list-expression)])
                  (make-LetExpression (@ new-operator body) bindings body))))))

      ;; parse-function-expression : -> Expression
      (define/public (parse-function-expression)
        (parse-function-definition #t))

      ;; parse-function-definition : boolean -> Term
      (define (parse-function-definition expression-context?)
        (let ([function-token (send t must-match 'function)]
              [name (let ([token (send t match 'ID)])
                      (and token (token->Identifier token)))])
          (when (and (not name)
                     (not expression-context?)
                     (not (allow-anonymous-function-source-elements?)))
            (let ([bad-token (send t peek-token)])
              (fail/loc (token-location bad-token)
                        (token-contents bad-token)
                        "missing required function identifier")))
          (send t must-match '\()
          (let ([args (map-tokens (sep '\, (until '\)))
                        (lambda ()
                          (let ([arg (send t read-token)])
                            (unless (eq? (token-type arg) 'ID)
                              (fail/loc (token-location arg)
                                        (token-contents arg)
                                        "expected formal parameter"))
                            (token->Identifier arg))))])
            (send t must-match '\))
            (send t must-match '\{)
            (let* ([body (parse-source-elements)]
                   [close-curly (send t must-match '\})]
                   [loc (@ function-token close-curly)])
              (cond
                [expression-context?
                 (make-FunctionExpression loc name args body)]
                [(not name)
                 (make-ExpressionStatement loc
                  (make-FunctionExpression loc name args body))]
                [else
                 (make-FunctionDeclaration loc name args body)])))))

      ;; parse-paren-expression : -> Expression
      (define (parse-paren-expression)
        (let ([open-paren (send t must-match '\()]
              [expr (parse-list-expression)]
              [close-paren (send t must-match '\))])
          (make-ParenExpression (@ open-paren close-paren)
                                expr)))

      ;; parse-expression : -> Expression
      (define/public (parse-expression)
        (parse-list-expression))

      ;; parse-single-expression : -> Expression
      (define (parse-single-expression)
        (parameterize ([disable-comma? #t])
          (parse-operand #f)))

      ;; parse-list-expression : -> Expression
      (define (parse-list-expression)
        (parameterize ([disable-comma? #f])
          (parse-operand #f)))

      ;; parse-argument-list : symbol -> (listof Expression)
      (define (parse-argument-list stop)
        (map-tokens (sep '\, (until stop))
          (lambda ()
            (parse-single-expression))))

      ;; parse-optional-argument-list : symbol -> (listof (optional Expression))
      (define (parse-optional-argument-list stop)
        (map-tokens (sep '\, (until stop))
          (lambda ()
            (and (not (memq (token-type (send t peek-token)) (list '\, stop)))
                 (parse-single-expression)))))

      ;; shift? : token (optional token) -> boolean
      (define (shift? operator context-operator)
        (or (not context-operator)
            (binds-tighter? operator context-operator)))

      ;; parse-core-operand : (optional token) -> Expression
      (define (parse-core-operand current-operator)
        (let* ([first-token (send t peek-token)]
               [left (case (token-type first-token)
                       [(function)
                        (parse-function-definition #t)]
                       [(null this true false ID NUMBER STRING REGEXP)
                        (send t read-token)
                        (case (token-type first-token)
                          [(null) (make-NullLiteral (token-location first-token))]
                          [(this) (make-ThisReference (token-location first-token))]
                          [(true) (make-BooleanLiteral (token-location first-token) #t)]
                          [(false) (make-BooleanLiteral (token-location first-token) #f)]
                          [(ID) (make-VarReference (token-location first-token) (token->Identifier first-token))]
                          [(NUMBER) (make-NumericLiteral (token-location first-token) (token-contents first-token))]
                          [(STRING) (make-StringLiteral (token-location first-token) (token-contents first-token))]
                          [(REGEXP)
                           (let ([contents (token-contents first-token)])
                             (make-RegexpLiteral (token-location first-token)
                                                 (regexp-contents-pattern contents)
                                                 (regexp-contents-global? contents)
                                                 (regexp-contents-case-insensitive? contents)))])]
                       [(delete void typeof ++ -- ! ~ UNARY)
                        (let* ([new-operator (send t read-token)]
                               [operand (parse-operand new-operator)])
                          (make-PrefixExpression (@ new-operator operand)
                                                 (token-contents first-token)
                                                 operand))]
                       [(new)
                        (let* ([new-operator (send t read-token)]
                               [constructor (parse-operand new-operator)])
                          (if (send t match '\()
                              (let ([args (parse-argument-list '\))]
                                    [close-paren (send t must-match '\))])
                                (make-NewExpression (@ new-operator close-paren)
                                                    constructor
                                                    args))
                              (make-NewExpression (@ new-operator constructor)
                                                  constructor
                                                  null)))]
                       [(let)
                        (parse-let-expression)]
                       [(\[)
                        (let ([new-operator (send t read-token)]
                              [elts (parse-optional-argument-list '\])]
                              [close-bracket (send t must-match '\])])
                          (make-ArrayLiteral (@ new-operator close-bracket)
                                             elts))]
                       [(\{)
                        (let ([open-curly (send t must-match '\{)]
                              [elts (map-tokens (sep '\, (until '\}))
                                      (lambda ()
                                        (let ([property (send t read-token)])
                                          (send t must-match ':)
                                          (let ([binding (parse-single-expression)])
                                            (cons (case (token-type property)
                                                    [(ID) (token->Identifier property)]
                                                    [(STRING) (make-StringLiteral (token-location property)
                                                                                  (token-contents property))]
                                                    [(NUMBER) (make-NumericLiteral (token-location property)
                                                                                   (token-contents property))]
                                                    [else (fail/loc (token-location property)
                                                                    (token-contents property)
                                                                    "invalid property id")])
                                                  binding)))))]
                              [close-curly (send t must-match '\})])
                          (make-ObjectLiteral (@ open-curly close-curly)
                                              elts))]
                       [(\()
                        (parse-paren-expression)]
                       [else (fail/loc (token-location first-token)
                                       (token-contents first-token)
                                       "unexpected expression token")])])
          (let loop ([left left])
            (case (token-type (send t peek-token/infix-operator))
              [(\.)
               (send t read-token/infix-operator)
               (let ([id (send t must-match 'ID)])
                 (loop (make-DotReference (@ left id)
                                          left
                                          (token->Identifier id))))]
              [(\[)
               (send t read-token/infix-operator)
               (let ([subscript (parse-list-expression)]
                     [close-bracket (send t must-match '\])])
                 (loop (make-BracketReference (@ left close-bracket)
                                              left
                                              subscript)))]
              [(\()
               ;; NOTE: new expressions take precedence over method calls
               (if (and current-operator (eq? (token-type current-operator) 'new))
                   left
                   (begin (send t read-token/infix-operator)
                          (let ([arguments (parse-argument-list '\))]
                                [close-paren (send t must-match '\))])
                            (loop (make-CallExpression (@ left close-paren)
                                                       left
                                                       arguments)))))]
              [else left]))))

      ;; parse-operand : (optional token) -> Expression
      (define (parse-operand current-operator)
        (let loop ([left (parse-core-operand current-operator)])
          (let ([next-token (send t peek-token/infix-operator)])
            (cond
              [(postfix-operator-token? next-token)
               (send t read-token)
               (loop (make-PostfixExpression (@ left next-token)
                                             left (token-contents next-token)))]
              [(and (infix-operator-token? next-token)
                    (shift? next-token current-operator))
               (let ([new-operator (send t read-token/infix-operator)])
                 (case (token-type next-token)
                   [(?)
                    (let ([consequent (parse-single-expression)]
                          [alternate (begin (send t must-match ':)
                                            (parameterize ([disable-comma? #t])
                                              (parse-operand new-operator)))])
                      (loop (make-ConditionalExpression (@ left alternate)
                                                        left consequent alternate)))]
                   [(\,)
                    (let ([right (parse-operand new-operator)])
                      (loop (make-ListExpression (@ left right) (list left right))))]
                   [(ASSIGN)
                    (let ([value (parse-operand new-operator)])
                      (loop (make-AssignmentExpression (@ left value)
                                                       left
                                                       (token-contents next-token)
                                                       value)))]
                   [else
                    (let ([right-operand (parse-operand new-operator)])
                      (loop (make-InfixExpression (@ left right-operand)
                                                  left
                                                  (token-contents next-token)
                                                  right-operand)))]))]
              [else left]))))

      (super-make-object tokenizer)))

  ;; input-source? : any -> boolean
  (define (input-source? x)
    (or (string? x)
        (path? x)
        (input-port? x)))

  ;; input-source->input-port : input-source -> input-port
  (define (input-source->input-port in)
    (cond
      [(string? in) (open-input-string in)]
      [(path? in) (open-input-file in)]
      [(input-port? in) in]))

  ;; input-source->parser : input-source -> parser
  (define (input-source->parser in)
    (make-object parser%
      (make-object tokenizer%
        (input-source->input-port in))))

  (define (dump-exn exn)
    (let ([t (exn:fail:javascript:syntax-source exn)])
      (send t show-state "*** FAILURE (~a)" (exn-message exn))
      (raise exn)))

  ;; parse-script : input-source -> (listof SourceElement)
  (define (parse-script src)
    (let ([parser (input-source->parser src)])
      (begin0 (send parser parse-source-elements)
              (send parser skip-empty-tokens))))

  ;; parse-expression : input-source -> Expression
  (define (parse-expression src)
    (let ([parser (input-source->parser src)])
      (begin0 (send parser parse-expression)
              (send parser skip-empty-tokens))))

  ;; parse-function-expression : input-source -> FunctionExpression
  (define (parse-function-expression src)
    (let ([parser (input-source->parser src)])
      (begin0 (send parser parse-function-expression)
              (send parser skip-empty-tokens))))

  ;; parse-source-element : input-source -> SourceElement
  (define (parse-source-element src)
    (let ([parser (input-source->parser src)])
      (begin0 (send parser parse-source-element)
              (send parser skip-empty-tokens))))

  (provide/contract
    [parser<%> interface?]
    [parser% (implementation?/c parser<%>)]
    [input-source? predicate/c]
    [input-source->input-port (input-source? . -> . input-port?)]
    [input-source->parser (input-source? . -> . (is-a?/c parser<%>))]
    [parse-script (input-source? . -> . (listof SourceElement?))]
    [parse-expression (input-source? . -> . Expression?)]
    [parse-function-expression (input-source? . -> . FunctionExpression?)]
    [parse-source-element (input-source? . -> . SourceElement?)]))
