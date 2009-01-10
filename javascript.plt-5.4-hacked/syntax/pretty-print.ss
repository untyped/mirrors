(module pretty-print mzscheme
  (require (lib "contract.ss")
           (lib "match.ss")
           (lib "list.ss")
           (lib "etc.ss")
           "ast.ss")

  ;; ===========================================================================
  ;; PARAMETERS
  ;; ===========================================================================

  (define current-indentation-width (make-parameter 4))
  (define current-indentation (make-parameter 0))
  #;(define current-column (make-parameter 0))
  (define collapse-lines? (make-parameter #f))
  (define collapse-simple-substatements? (make-parameter #f))

  (define pretty-printers/Expression (make-parameter null))
  (define pretty-printers/Statement (make-parameter null))
  (define pretty-printers/ExpressionList (make-parameter null))
  (define pretty-printers/StatementList (make-parameter null))
  ;; TODO:
  ;;   - pretty-printers/Declaration
  ;;   - pretty-printers/SourceElementList (?)
  ;;   - pretty-printers/InitializerList

  ;; ===========================================================================
  ;; PRECEDENCES
  ;; ===========================================================================

  ;; TODO: shouldn't `new' bind tighter than call? make a test case

  (define expression-precedences
    '(;; binds loosest
      (AssignmentExpression)
      (ConditionalExpression)
      (InfixExpression)
      (PrefixExpression)
      (PostfixExpression)
      (NewExpression)
      (FunctionExpression BracketReference DotReference CallExpression)
      (StringLiteral NumericLiteral BooleanLiteral NullLiteral RegexpLiteral
       ThisReference VarReference
       ArrayLiteral ObjectLiteral)
      (ParenExpression)
      ;; binds tightest
      ))

  ;; expression-precedence : (union struct-type Expression) -> natural-number
  (define (expression-precedence e)
    (let* ([type (cond
                   [(Expression? e) (let-values ([(type _) (struct-info e)]) type)]
                   [(struct-type? e) e])]
           [type-name (let-values ([(name _2 _3 _4 _5 _6 _7 _8) (struct-type-info type)]) name)])
      (let loop ([precedences expression-precedences] [n 0])
        (when (empty? precedences)
          (error 'expression-precedence "no precedence for ~a" type))
        (if (memq type-name (car precedences))
            n
            (loop (cdr precedences) (add1 n))))))

  (define infix-expression-precedence (expression-precedence struct:InfixExpression))

  ;; TODO: could make this overrideable, but maybe not important

  ;; binds-tighter? : Expression * Expression -> boolean
  (define (binds-tighter? e1 e2)
    (cond
      [(not (Expression? e1)) #f]
      [(not (Expression? e2)) #t]
      [else  (let ([p1 (expression-precedence e1)]
                   [p2 (expression-precedence e2)])
               (or (> p1 p2)
                   (and (= p1 p2 infix-expression-precedence)
                        (infix-binds-tighter? e1 e2))))]))

  (define operator-precedences
    '(;; binds loosest
      (\|\|)
      (&&)
      (\|)
      (^)
      (&)
      (== != === !==)
      (< > <= >= instanceof in)
      (<< >> >>>)
      (+ -)
      (* / %)
      ;; binds tightest
      ))

  ;; operator-precedence : (union InfixExpression symbol) -> natural-number
  (define (operator-precedence e)
    (let ([operator (cond
                      [(InfixExpression? e) (InfixExpression-operator e)]
                      [(symbol? e) e])])
      (let loop ([precedences operator-precedences] [n 0])
        (when (empty? precedences)
          (error 'operator-precedence "No precedence for ~S" operator))
        (if (memq operator (car precedences))
            n
            (loop (cdr precedences) (add1 n))))))

  ;; infix-binds-tighter? : InfixExpression * InfixExpression -> boolean
  (define (infix-binds-tighter? e1 e2)
    (> (operator-precedence e1)
       (operator-precedence e2)))

  ;; ===========================================================================
  ;; COMBINATORS
  ;; ===========================================================================

  (define-syntax with-indentation
    (syntax-rules ()
      [(_ e0 e1 ...)
       (parameterize ([current-indentation (if (collapse-lines?)
                                               (current-indentation)
                                               (+ (current-indentation) (current-indentation-width)))])
         e0 e1 ...)]))

  (define (pretty-newline)
    (if (not (collapse-lines?))
        (begin (newline)
               (display (make-string (current-indentation) #\space))
               #;(current-column (current-indentation)))
        (begin (display " ")
               #;(current-column (add1 (current-column))))))

  (define (pretty-display v)
    (let ([str (format "~a" v)])
      (display str)
      #;(current-column (+ (current-column) (string-length str)))))

  ;; comma-separate : (a -> any) * (listof a) [parameter] -> any
  (define comma-separate
    (opt-lambda (proc elts [param #f])
      (match elts
        [() (void)]
        [(elt) (proc elt)]
        [(elt0 elts ...)
         (proc elt0)
         (for-each (lambda (elt)
                     (pretty-display ", ")
                     (proc elt))
                   elts)]
        [(? (lambda _ param))
         (pretty-print/extensions elts (param))])))

  (define (fail-match x)
    (match x
      [(? (lambda _ #f)) #f]))

;  (define failed-match (let-struct failed-match ()
;                         (make-failed-match)))

  ;; for-each/X : (a -> any) (union (listof a) b) parameter) -> any
  (define (for-each/X proc elts param)
    (if (list? elts)
        (for-each proc elts)
        (pretty-print/extensions elts (param))))

  (define (pretty-print/extensions x extensions)
    (if (null? extensions)
        (fail-match x)
        (with-handlers ([exn:misc:match? (lambda (exn)
                                           (pretty-print/extensions x (cdr extensions)))])
          ((car extensions) x))))

  ;; ===========================================================================
  ;; PRETTY PRINTER
  ;; ===========================================================================

  ;; TODO: design this as a combinator library, e.g.:
  ;; 
  ;;       (pretty-printf
  ;;          "function " name "(" (~c ~a args) ") {"
  ;;              (~i (~e pretty-print-function-element body)) ~n
  ;;          "}")
  ;; 
  ;; The combinators may or may not be pure. I could do various acrobatics to
  ;; design the library to be context-passing in order to achieve linear
  ;; efficiency, but it's probably acceptable just to keep using ports.

  ;; pretty-print-source-element : SourceElement -> any
  (define (pretty-print-source-element elt)
    (pretty-newline)
    (pretty-print elt))

  ;; pretty-print-variable-initializer : VariableInitializer -> any
  (define (pretty-print-variable-initializer init)
    (match init
      [($ VariableInitializer _ id init)
       (pretty-print-identifier id)
       (when init
         (pretty-display " = ")
         (pretty-print-expression init))]))

  ;; pretty-print-initializer : VariableInitializer -> any
;  (define (pretty-print-initializer init)
;    (match init
;      [($ VariableInitializer _ id #f)
;       (pretty-print-identifier id)]
;      [($ VariableInitializer _ id init)
;       (pretty-print-identifier id)
;       (pretty-display " = ")
;       (pretty-print-expression init)]))

  ;; pretty-print-declaration : Declaration -> any
  (define (pretty-print-declaration decl)
    (match decl
      [($ FunctionDeclaration _ name args body)
       (pretty-display "function ")
       (pretty-print-identifier name)
       (pretty-display "(")
       (comma-separate pretty-print-identifier args)
       (pretty-display ") {")
       (with-indentation
         (for-each/X pretty-print-source-element body pretty-printers/StatementList))
       (pretty-newline)
       (pretty-display "}")]
      ;; TODO: LetDeclaration
      [($ VariableDeclaration _ bindings)
       (pretty-display "var ")
       (comma-separate pretty-print-variable-initializer bindings)
       (pretty-display ";")]))

  ;; pretty-print-expression : Expression -> any
  (define (pretty-print-expression expr)
    (with-handlers ([exn:misc:match? (lambda (exn)
                                       (pretty-print/extensions expr (pretty-printers/Expression)))])
      (match expr
        [($ StringLiteral _ value)
         (pretty-display (format "~v" value))] ;; TODO: use the real lexical definition
        [($ NumericLiteral _ value)
         (pretty-display (format "~a" value))] ;; TODO: use the real lexical definition
        [($ BooleanLiteral _ value)
         (pretty-display (if value "true" "false"))]
        [($ NullLiteral _)
         (pretty-display "null")]
        [($ RegexpLiteral _ pattern g? i?)
         (pretty-display "/")
         (pretty-display pattern)
         (pretty-display "/")
         (when g?
           (pretty-display "g"))
         (when i?
           (pretty-display "i"))]
        [($ ArrayLiteral _ elements)
         (if (null? elements)
             (pretty-display "[]")
             (begin (pretty-display "[ ")
                    (when (car elements)
                      (pretty-print-expression (car elements)))
                    (for-each/X (lambda (element)
                                  (pretty-display ",")
                                  (when element
                                    (pretty-display " ")
                                    (pretty-print-expression element)))
                                (cdr elements)
                                pretty-printers/ExpressionList)
                    (pretty-display " ]")))]
        [($ ObjectLiteral _ properties)
         (if (null? properties)
             (pretty-display "{}")
             (begin (pretty-display "{")
                    (with-indentation
                     (pretty-newline)
                     (pretty-print-property (car properties))
                     (for-each (lambda (property)
                                 (pretty-display ",")
                                 (pretty-newline)
                                 (pretty-print-property property))
                               (cdr properties)))
                    (pretty-newline)
                    (pretty-display "}")))]
        [($ ThisReference _)
         (pretty-display "this")]
        [($ VarReference _ id)
         (pretty-print-identifier id)]
        [($ BracketReference _ container key)
         (pretty-print-subexpression container expr)
         (pretty-display "[")
         (pretty-print-expression key)
         (pretty-display "]")]
        [($ DotReference _ container id)
         (pretty-print-subexpression container expr)
         (pretty-display ".")
         (pretty-print-identifier id)]
        [($ NewExpression _ constructor arguments)
         (pretty-display "new ")
         (pretty-print-subexpression constructor expr)
         (pretty-display "(")
         (comma-separate pretty-print-expression arguments pretty-printers/ExpressionList)
         (pretty-display ")")]
        [($ PostfixExpression _ expression operator)
         (pretty-print-subexpression expression expr)
         (pretty-display operator)]
        [($ PrefixExpression _ operator expression)
         (pretty-display operator)
         (pretty-print-subexpression expression expr)]
        [($ InfixExpression _ left operator right)
         (if (InfixExpression? left)
             (if (infix-binds-tighter? expr left)
                 (begin (pretty-display "(")
                        (pretty-print-expression left)
                        (pretty-display ")"))
                 (pretty-print-expression left))
             (pretty-print-subexpression left expr))
         (pretty-display " ")
         (pretty-display operator)
         (pretty-display " ")
         ;; We don't reassociate because of e.g. overloading of the + operator.
         ;; We could potentially reassociate some operators, but this is enough.
         (if (binds-tighter? right expr)
             (pretty-print-expression right)
             (begin (pretty-display "(")
                    (pretty-print-expression right)
                    (pretty-display ")")))]
        [($ ConditionalExpression _ test consequent alternate)
         (pretty-print-subexpression test expr)
         (pretty-display " ? ")
         (pretty-print-subexpression consequent expr)
         (pretty-display " : ")
         (pretty-print-subexpression alternate expr)]
        [($ AssignmentExpression _ lhs operator rhs)
         (pretty-print-subexpression lhs expr)
         (pretty-display " ")
         (pretty-display operator)
         (pretty-display " ")
         (pretty-print-subexpression rhs expr)]
        [($ FunctionExpression _ name args body)
         (pretty-display "function")
         (when name
           (pretty-display " ")
           (pretty-print-identifier name))
         (pretty-display "(")
         (comma-separate pretty-print-identifier args)
         (pretty-display ") {")
         (with-indentation
          (for-each/X pretty-print-source-element body pretty-printers/StatementList))
         (pretty-newline)
         (pretty-display "}")]
        [($ LetExpression _ bindings body)
         (pretty-display "let (")
         (comma-separate pretty-print-variable-initializer bindings)
         (pretty-display ")")
         (with-indentation
          (pretty-print-expression body))]
        [($ CallExpression _ method args)
         (pretty-print-subexpression method expr)
         (pretty-display "(")
         (comma-separate pretty-print-expression args pretty-printers/ExpressionList)
         (pretty-display ")")]
        [($ ParenExpression _ expr)
         (pretty-display "(")
         (pretty-print-expression expr)
         (pretty-display ")")]
        [($ ListExpression _ exprs)
         (comma-separate pretty-print-expression exprs pretty-printers/ExpressionList)]
        )))

  ;; pretty-print-subexpression : Expression * Expression -> any
  (define (pretty-print-subexpression expr parent)
    (if (binds-tighter? parent expr)
        (begin (pretty-display "(")
               (pretty-print-expression expr)
               (pretty-display ")"))
        (pretty-print-expression expr)))

  ;; pretty-print-statement : Statement -> any
  ;; POSTCONDITIONS:
  ;;   - statement output includes its own semicolon if appropriate
  ;;   - statement output is not newline-terminated
  (define (pretty-print-statement stmt)
    (with-handlers ([exn:misc:match? (lambda (exn)
                                       (pretty-print/extensions stmt (pretty-printers/Statement)))])
      (match stmt
        [($ BlockStatement _ statements)
         (if (null? statements)
             (pretty-display "{}")
             (begin
               (with-indentation
                (pretty-display "{")
                (pretty-newline)
                (pretty-print-substatement (car statements))
                (for-each/X (lambda (statement)
                              (pretty-newline)
                              (pretty-print-substatement statement))
                            (cdr statements)
                            pretty-printers/StatementList))
               (pretty-newline)
               (pretty-display "}")))]
        [($ EmptyStatement _)
         (pretty-display ";")]
        [($ ExpressionStatement _ expression)
         (pretty-print-expression expression)
         (pretty-display ";")]
        [($ IfStatement _ test consequent alternate)
         (pretty-display "if (")
         (pretty-print-expression test)
         (pretty-display ")")
         (pretty-print-nested-substatement consequent)
         (pretty-newline)
         (cond
           [(IfStatement? alternate)
            (pretty-display "else ")
            (pretty-print-statement alternate)]
           [alternate
            (pretty-display "else")
            (pretty-print-nested-substatement alternate)])]
        [($ DoWhileStatement _ body test)
         (pretty-display "do")
         (if (BlockStatement? body)
             (begin (pretty-display " ")
                    (pretty-print-substatement body)
                    (pretty-display " "))
             (begin (with-indentation
                     (pretty-newline)
                     (pretty-print-substatement body))
                    (pretty-newline)))
         (pretty-display "while (")
         (pretty-print-expression test)
         (pretty-display ");")]
        [($ WhileStatement _ test body)
         (pretty-display "while (")
         (pretty-print-expression test)
         (pretty-display ")")
         (pretty-print-nested-substatement body)]
        [($ ForStatement _ init test incr body)
         (pretty-display "for (")
         (cond
           [(Expression? init)
            (pretty-print-expression init)]
           [(VariableDeclaration? init)
            (pretty-display "var ")
            (comma-separate pretty-print-variable-initializer (VariableDeclaration-bindings init))])
         (pretty-display ";")
         (when test
           (pretty-display " ")
           (pretty-print-expression test))
         (pretty-display ";")
         (when incr
           (pretty-display " ")
           (pretty-print-expression incr))
         (pretty-display ")")
         (pretty-print-nested-substatement body)]
        [($ ForInStatement _ lhs container body)
         (pretty-display "for (")
         (if (Expression? lhs)
             (pretty-print-expression lhs)
             (begin (pretty-display "var ")
                    (comma-separate pretty-print-variable-initializer (VariableDeclaration-bindings lhs))))
         (pretty-display " in ")
         (pretty-print-expression container)
         (pretty-display ")")
         (pretty-print-nested-substatement body)]
        [($ ContinueStatement _ label)
         (pretty-display "continue")
         (when label
           (pretty-display " ")
           (pretty-print-identifier label))
         (pretty-display ";")]
        [($ BreakStatement _ label)
         (pretty-display "break")
         (when label
           (pretty-display " ")
           (pretty-print-identifier label))
         (pretty-display ";")]
        [($ ReturnStatement _ value)
         (pretty-display "return")
         (when value
           (pretty-display " ")
           (pretty-print-expression value))
         (pretty-display ";")]
        [($ WithStatement _ context body)
         (pretty-display "with (")
         (pretty-print-expression context)
         (pretty-display ")")
         (with-indentation
          (pretty-newline)
          (pretty-print-substatement body))]
        [($ SwitchStatement _ expression cases)
         (pretty-display "switch (")
         (pretty-print-expression expression)
         (pretty-display ") {")
         (with-indentation
          (pretty-newline)
          (pretty-print-case-clause (car cases))
          (for-each (lambda (case)
                      (pretty-newline)
                      (pretty-print-case-clause case))
                    (cdr cases)))
         (pretty-newline)
         (pretty-display "}")]
        [($ LabelledStatement _ label statement)
         (pretty-print-identifier label)
         (pretty-display ":")
         (with-indentation
          (pretty-newline)
          (pretty-print-substatement statement))]
        [($ ThrowStatement _ value)
         (pretty-display "throw ")
         (pretty-print-expression value)
         (pretty-display ";")]
        [($ TryStatement _ body catches finally)
         (pretty-display "try")
         (pretty-print-nested-substatement body)
         (for-each (lambda (catch)
                     (pretty-newline)
                     (match-let ([($ CatchClause _ id body) catch])
                       (pretty-display "catch (")
                       (pretty-print-identifier id)
                       (pretty-display ")")
                       (pretty-print-nested-substatement body)))
                   catches)
         (when finally
           (pretty-newline)
           (pretty-display "finally")
           (pretty-print-nested-substatement finally))]
        )))

  ;; pretty-print-nested-substatement : SubStatement -> any
  ;; PRECONDITION:
  ;;   - starts on the same line as its containing statement
  (define (pretty-print-nested-substatement body)
    (cond
      [(EmptyStatement? body)
       (pretty-display ";")]
      [(or (BlockStatement? body)
           (collapse-simple-substatements?))
       (pretty-display " ")
       (pretty-print-substatement body)]
      [else
       (with-indentation
        (pretty-newline)
        (pretty-print-substatement body))]))

  ;; pretty-print-substatement : SubStatement -> any
  (define (pretty-print-substatement statement)
    (if (Declaration? statement)
        (pretty-print-declaration statement)
        (pretty-print-statement statement)))

  ;; pretty-print-case-clause : CaseClause -> any
  (define (pretty-print-case-clause case)
    (let ([question (CaseClause-question case)]
          [answer (CaseClause-answer case)])
      (if question
          (pretty-print-expression question)
          (pretty-display "default"))
      (pretty-display ":")
      (if (= (length answer) 1)
          (pretty-print-nested-substatement (car answer))
          (with-indentation
            (pretty-newline)
            (for-each/X pretty-print-substatement answer pretty-printers/StatementList)))))

  ;; pretty-print-property : Property -> any
  (define (pretty-print-property pair)
    (let ([property (car pair)]
          [value (cdr pair)])
      (if (Identifier? property)
          (pretty-print-identifier property)
          (pretty-print property))
      (pretty-display ": ")
      (pretty-print-expression value)))

  ;; pretty-print-identifier : Identifier -> any
  (define (pretty-print-identifier id)
    (pretty-display (Identifier-name id)))

  ;; pretty-print : Term/X -> any
  (define (pretty-print term)
    (cond
      [(Declaration? term) (pretty-print-declaration term)]
      [(Statement/X? term) (pretty-print-statement term)]
      [(Expression/X? term) (pretty-print-expression term)]))

  ;; pretty-format : Term -> string
  (define (pretty-format term)
    (let ([string-port (open-output-string)])
      (with-handlers ([exn? (lambda (exn)
                              (printf "Got as far as:~n~s~n" (get-output-string string-port))
                              (raise exn))])
        (parameterize ([current-output-port string-port])
          (pretty-print term)
          (get-output-string string-port)))))

  (provide/contract
    [pretty-print (Term/X? . -> . any)]
    [pretty-format (Term/X? . -> . string?)])

  (provide pretty-printers/Expression pretty-printers/Statement pretty-printers/ExpressionList pretty-printers/StatementList)
  (provide current-indentation-width current-indentation collapse-lines? collapse-simple-substatements?)
  
  ; [DJG] Extra provides for javascript/render.ss:
  (provide for-each/X pretty-newline pretty-print-substatement))
