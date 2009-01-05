(module ast-utils mzscheme
  (require (planet "contract-utils.ss" ("cobbe" "contract-utils.plt" 1 0))
           (lib "contract.ss")
           (lib "etc.ss")
           "ast-core.ss"
           "../config.ss"
           "token.ss")

  (define (Property? x)
    (or (Identifier? x)
        (StringLiteral? x)
        (NumericLiteral? x)))

  (define (SourceElement? x)
    (or (Statement/X? x)
        (Declaration? x)))

  (define (SubStatement? x)
    (or (Statement? x)
        (and (Declaration? x)
             (not (FunctionDeclaration? x)))
        (and (allow-nested-function-declarations?)
             (FunctionDeclaration? x))))

  (define (extensible-predicate base param)
    (let ([base? (if (flat-contract? base)
                     (flat-contract-predicate base)
                     base)])
      (lambda (x)
        (or (base? x)
            (recur matches-any? ([predicates (param)])
              (and (pair? predicates)
                   (or ((car predicates) x)
                       (matches-any? (cdr predicates)))))))))

  ;; TODO: make custom "with-X-predicate" forms instead of exposing these
  (define Expression-predicates (make-parameter null))
  (define Statement-predicates (make-parameter null))
  (define ExpressionList-predicates (make-parameter null))
  (define StatementList-predicates (make-parameter null))
  ;; TODO:
  ;;   - Declaration-predicates
  ;;   - SourceElementList-predicates (?)
  ;;   - InitializerList-predicates

  (define Expression/X?
    (extensible-predicate Expression? Expression-predicates))

  (define Statement/X?
    (extensible-predicate Statement? Statement-predicates))

  (define SubStatement/X?
    (extensible-predicate SubStatement? Statement-predicates))

  (define ExpressionList/X?
    (extensible-predicate (listof Expression/X?) ExpressionList-predicates))

  (define StatementList/X?
    (extensible-predicate (listof Statement/X?) StatementList-predicates))

  (define SubStatementList/X?
    (extensible-predicate (listof SubStatement/X?) StatementList-predicates))

  (define (Term/X? x)
    (or (Declaration? x)
        (Statement/X? x)
        (Expression/X? x)))

  (define (Identifier=? id1 id2)
    (eq? (Identifier-name id1)
         (Identifier-name id2)))

  (define postfix-operators '(++ --))
  (define prefix-operators '(delete void typeof ++ -- + - ~ !))
  (define infix-operators '(* / % + -
                            << >> >>> < > <= >=
                            instanceof in
                            == != === !==
                            & ^ \|
                            && \|\|))
  (define assignment-operators '(= *= /= %= += -= <<= >>= >>>= &= ^= \|=))

  ;; assignment-operator->infix-operator : assignment-operator -> (optional infix-operator)
  (define (assignment-operator->infix-operator aop)
    (and (not (eq? aop '=))
         (let* ([aop-str (symbol->string aop)]
                [op-str (substring aop-str 0 (sub1 (string-length aop-str)))])
           (string->symbol op-str))))

  (define (postfix-operator? x) (and (memq x postfix-operators) #t))
  (define (prefix-operator? x) (and (memq x prefix-operators) #t))
  (define (infix-operator? x) (and (memq x infix-operators) #t))
  (define (assignment-operator? x) (and (memq x assignment-operators) #t))

  (define PostfixOperator/c (apply symbols postfix-operators))
  (define PrefixOperator/c (apply symbols prefix-operators))
  (define InfixOperator/c (apply symbols infix-operators))
  (define AssignmentOperator/c (apply symbols assignment-operators))

  ;; ===========================================================================
  ;; LOCATION INFORMATION
  ;; ===========================================================================

  ;; has-location? : any -> boolean
  (define (has-location? x)
    (or (token? x) (Term? x)))

  ;; ast-location : has-location -> (optional region)
  (define (ast-location ast)
    (cond
      [(token? ast) (token-location ast)]
      [(Term? ast) (Term-location ast)]
      [else (error 'ast-location "not an ast node")]))

  ;; ast-source : has-location -> (optional any)
  (define (ast-source t)
    (cond
      [(ast-location t) => region-source]
      [else #f]))

  ;; ast-start : has-location -> (optional position)
  (define (ast-start t)
    (cond
      [(ast-location t) => region-start]
      [else #f]))

  ;; ast-end : has-location -> (optional position)
  (define (ast-end t)
    (cond
      [(ast-location t) => region-end]
      [else #f]))

  ;; @ : (optional has-location) (optional has-location) -> (optional region)
  (define (@ start end)
    (and start
         end
         (let ([source (ast-source start)]
               [start (ast-start start)]
               [end (ast-end end)])
           (and start end (make-region source start end)))))

  (provide/contract
    [has-location? predicate/c]
    [ast-location (has-location? . -> . (optional/c region?))]
    [ast-source (has-location? . -> . (optional/c any/c))]
    [ast-start (has-location? . -> . (optional/c position?))]
    [ast-end (has-location? . -> . (optional/c position?))]
    [@ ((optional/c has-location?) (optional/c has-location?) . -> . (optional/c region?))])

  (provide/contract
    [Property? predicate/c]
    [SubStatement? predicate/c]
    [SourceElement? predicate/c]
    [Identifier=? (Identifier? Identifier? . -> . boolean?)])

  (provide/contract
    [Expression-predicates parameter?]
    [Statement-predicates parameter?]
    [ExpressionList-predicates parameter?]
    [StatementList-predicates parameter?])

  (provide/contract
    [Expression/X? predicate/c]
    [Statement/X? predicate/c]
    [SubStatement/X? predicate/c]
    [ExpressionList/X? predicate/c]
    [StatementList/X? predicate/c]
    [SubStatementList/X? predicate/c])

  (provide/contract
    [Term/X? predicate/c])

  (provide/contract
    [postfix-operators (listof symbol?)]
    [prefix-operators (listof symbol?)]
    [infix-operators (listof symbol?)]
    [assignment-operators (listof symbol?)]
    [assignment-operator->infix-operator (assignment-operator? . -> . (optional/c infix-operator?))]
    [postfix-operator? predicate/c]
    [prefix-operator? predicate/c]
    [infix-operator? predicate/c]
    [assignment-operator? predicate/c]
    [PostfixOperator/c flat-contract?]
    [PrefixOperator/c flat-contract?]
    [InfixOperator/c flat-contract?]
    [AssignmentOperator/c flat-contract?]))