(module sexp mzscheme
  (require (planet "contract-utils.ss" ("cobbe" "contract-utils.plt" 1 0))
           (all-except (lib "list.ss" "srfi" "1") any)
           (lib "contract.ss")
           (lib "match.ss")
           "ast.ss"
           "../config.ss")

  ;; TODO: deal with keyword/variable conflicts (array, object, field-ref, etc.)

  ;; sexp? : any -> boolean
  (define (sexp? x)
    (or (symbol? x)
        (number? x)
        (string? x)
        (boolean? x)
        (null? x)
        (and (pair? x) (andmap sexp? x))))

  ;; ===========================================================================
  ;; SEXP PARSING
  ;; ===========================================================================

  ;; TODO: allow multiple arity infix operators in sexps

  ;; TODO: lots more parse error checking
  ;;   - check syntax of subforms
  ;;   - better error messages when falling off end of match

  ;; sexp->Expression : sexp -> Expression
  (define (sexp->Expression sexp)
    (match sexp
      [(? string?) (make-StringLiteral #f sexp)]
      [('regexp pattern global? case-insensitive?)
       (make-RegexpLiteral #f pattern global? case-insensitive?)]
      [(? number?) (make-NumericLiteral #f sexp)]
      [(? boolean?) (make-BooleanLiteral #f sexp)]
      ['null (make-NullLiteral #f)]
      [('array elts ...)
       (make-ArrayLiteral #f (map sexp->ArrayElement elts))]
      [('object [props vals] ...)
       (make-ObjectLiteral #f (map (lambda (prop val)
                                     (cons (sexp->Property prop) (sexp->Expression val)))
                                   props
                                   vals))]
      ['this (make-ThisReference #f)]
      [(? symbol?) (make-VarReference #f (make-Identifier #f sexp))]
      [('field-ref container key)
       (make-BracketReference #f (sexp->Expression container)
                                 (sexp->Expression key))]
      [('field container id)
       (make-DotReference #f (sexp->Expression container) (make-Identifier #f id))]
      [('new constructor args ...)
       (make-NewExpression #f (sexp->Expression constructor) (map sexp->Expression args))]
      [('prefix op expr)
       (make-PrefixExpression #f op (sexp->Expression expr))]
      [('postfix expr op)
       (make-PostfixExpression #f (sexp->Expression expr) op)]
      [((and op (? infix-operator?)) left right)
       (make-InfixExpression #f (sexp->Expression left) op (sexp->Expression right))]
      [('? test consequent alternate)
       (make-ConditionalExpression #f (sexp->Expression test)
                                      (sexp->Expression consequent)
                                      (sexp->Expression alternate))]
      [((and op (? assignment-operator?)) left right)
       (make-AssignmentExpression #f (sexp->Expression left) op (sexp->Expression right))]
      [('function (and name (? symbol?)) ((and args (? symbol?)) ...) body ...)
       (make-FunctionExpression #f (make-Identifier #f name)
                                   (map (lambda (arg)
                                          (make-Identifier #f arg))
                                        args)
                                   (map sexp->SourceElement body))]
      [('function ((and args (? symbol?)) ...) body ...)
       (make-FunctionExpression #f #f
                                   (map (lambda (arg)
                                          (make-Identifier #f arg))
                                        args)
                                   (map sexp->SourceElement body))]
      [('begin e ...)
       (make-ListExpression #f (map sexp->Expression e))]
      [(method args ...)
       (let ([method-expr (sexp->Expression method)])
         (if (FunctionExpression? method-expr)
             (make-CallExpression #f (make-ParenExpression #f method-expr) (map sexp->Expression args))
             (make-CallExpression #f method-expr (map sexp->Expression args))))]
      [_ (error 'sexp->Expression "invalid sexp")]))

  ;; sexp->Property : sexp -> Property
  (define (sexp->Property sexp)
    (cond
      [(symbol? sexp) (make-Identifier #f sexp)]
      [(string? sexp) (make-StringLiteral #f sexp)]
      [(number? sexp) (make-NumericLiteral #f sexp)]))

  ;; sexp->ArrayElement : sexp -> (optional Expression)
  (define (sexp->ArrayElement sexp)
    (and (not (null? sexp))
         (sexp->Expression sexp)))

  ;; sexp->SourceElement : sexp -> SourceElement
  (define (sexp->SourceElement sexp)
    (match sexp
      [('function (and name (? symbol?)) ((and args (? symbol?)) ...) body ...)
       (make-FunctionDeclaration #f (make-Identifier #f name)
                                    (map (lambda (arg)
                                           (make-Identifier #f arg))
                                         args)
                                    (map sexp->SourceElement body))]
      [_ (sexp->SubStatement sexp)]))

  ;; sexp->SubStatement : sexp -> SubStatement
  (define (sexp->SubStatement sexp)
    (match sexp
      [('function (and name (? symbol?)) ((and args (? symbol?)) ...) body ...)
       ;; TODO: better error message
       (when (not (allow-nested-function-declarations?))
         (error 'sexp->SubStatement "illegally nested function definition"))
       (make-FunctionDeclaration #f (make-Identifier #f name)
                                    (map (lambda (arg)
                                           (make-Identifier #f arg))
                                         args)
                                    (map sexp->SourceElement body))]
      [('var decls ...)
       (make-VariableDeclaration #f (map sexp->VariableInitializer decls))]
      ;; TODO: [('let decls ...) _]
      [_ (sexp->Statement sexp)]))

  ;; sexp->VariableInitializer : sexp -> VariableInitializer
  (define (sexp->VariableInitializer sexp)
    (match sexp
      [(? symbol?)
       (make-VariableInitializer #f (make-Identifier #f sexp) #f)]
      [[id value]
       (make-VariableInitializer #f (make-Identifier #f id) (sexp->Expression value))]
      [_ (error 'sexp->VariableInitializer "invalid sexp")]))

  ;; sexp->CaseClause : sexp -> CaseClause
  (define (sexp->CaseClause sexp)
    (match sexp
      [('default stmts ...)
       (make-CaseClause #f #f (map sexp->SubStatement stmts))]
      [('case value stmts ...)
       (make-CaseClause #f (sexp->Expression value) (map sexp->SubStatement stmts))]
      [_ (error 'sexp->Expression "invalid sexp")]))

  ;; sexp->CatchClause : sexp -> CatchClause
  (define (sexp->CatchClause sexp)
    (match sexp
      [('catch id body)
       (make-CatchClause #f (make-Identifier #f id) (sexp->BlockStatement body))]
      [_ (error 'sexp->Expression "invalid sexp")]))

  ;; sexp->BlockStatement : sexp -> BlockStatement
  (define (sexp->BlockStatement sexp)
    (match sexp
      [('block elts ...)
       (make-BlockStatement #f (map sexp->SubStatement elts))]
      [_ (error 'sexp->Expression "invalid sexp")]))

  ;; sexp->Statement : sexp -> Statement
  (define (sexp->Statement sexp)
    (match sexp
      [('block elts ...)
       (sexp->BlockStatement sexp)]
      [()
       (make-EmptyStatement #f)]
      [('if test consequent alternate)
       (make-IfStatement #f (sexp->Expression test)
                            (sexp->SubStatement consequent)
                            (sexp->SubStatement alternate))]
      [('if test consequent)
       (make-IfStatement #f (sexp->Expression test)
                            (sexp->SubStatement consequent)
                            #f)]
      [('do body test)
       (make-DoWhileStatement #f (sexp->SubStatement body)
                                 (sexp->Expression test))]
      [('while test body)
       (make-WhileStatement #f (sexp->Expression test)
                               (sexp->SubStatement body))]
      [('for ('var inits ...) test incr body)
       (make-ForStatement #f (make-VariableDeclaration #f (map sexp->VariableInitializer inits))
                             (sexp->Expression test)
                             (sexp->Expression incr)
                             (sexp->SubStatement body))]
      [('for init test incr body)
       (make-ForStatement #f (sexp->Expression init)
                             (sexp->Expression test)
                             (sexp->Expression incr)
                             (sexp->SubStatement body))]
      [('for-in (('var var) container) body)
       (make-ForInStatement #f (make-VariableDeclaration #f (list (make-VariableInitializer #f (make-Identifier #f var) #f)))
                               (sexp->Expression container)
                               (sexp->SubStatement body))]
      [('for-in (lhs container) body)
       (make-ForInStatement #f (sexp->Expression lhs)
                               (sexp->Expression container)
                               (sexp->SubStatement body))]
      [('continue label)
       (make-ContinueStatement #f (make-Identifier #f label))]
      [('continue)
       (make-ContinueStatement #f #f)]
      [('break label)
       (make-BreakStatement #f (make-Identifier #f label))]
      [('break)
       (make-BreakStatement #f #f)]
      [('return value)
       (make-ReturnStatement #f (sexp->Expression value))]
      [('return)
       (make-ReturnStatement #f #f)]
      [('with context body)
       (make-WithStatement #f (sexp->Expression context)
                              (sexp->SubStatement body))]
      [('switch test cases ...)
       (make-SwitchStatement #f (sexp->Expression test)
                                (map sexp->CaseClause cases))]
      [('label label stmt)
       (make-LabelledStatement #f (make-Identifier #f label) (sexp->SubStatement stmt))]
      [('throw value)
       (make-ThrowStatement #f (sexp->Expression value))]
      [('try body clauses ...)
       (match (last clauses)
         [('finally finally)
          (make-TryStatement #f (sexp->BlockStatement body)
                                (map sexp->CatchClause (drop-right clauses 1))
                                (sexp->BlockStatement finally))]
         [_ (make-TryStatement #f (sexp->BlockStatement body)
                                  (map sexp->CatchClause clauses)
                                  #f)])]
      [_ (make-ExpressionStatement #f (sexp->Expression sexp))]))

  ;; ===========================================================================
  ;; SEXP GENERATION
  ;; ===========================================================================

  ;; Expression->sexp : Expression -> sexp
  (define (Expression->sexp expr)
    (match expr
      [($ StringLiteral _ str) str]
      [($ RegexpLiteral _ pattern global? case-insensitive?)
       `(regexp ,pattern ,global? ,case-insensitive?)]
      [($ NumericLiteral _ n) n]
      [($ BooleanLiteral _ b)
       (if b #t #f)]
      [($ NullLiteral _) 'null]
      [($ ArrayLiteral _ elts)
       `(array ,@(map ArrayElement->sexp elts))]
      [($ ObjectLiteral _ ((props . values) ...))
       `(object ,@(map (lambda (prop val)
                         (list (Property->sexp prop) (Expression->sexp val)))
                       props
                       values))]
      [($ ThisReference _) 'this]
      [($ VarReference _ ($ Identifier _ id)) id]
      [($ BracketReference _ container key)
       `(field-ref ,(Expression->sexp container)
                   ,(Expression->sexp key))]
      [($ DotReference _ container ($ Identifier _ id))
       `(field ,(Expression->sexp container) ,id)]
      [($ NewExpression _ constructor args)
       `(new ,(Expression->sexp constructor) ,@(map Expression->sexp args))]
      [($ PrefixExpression _ op expr)
       `(prefix ,op ,(Expression->sexp expr))]
      [($ PostfixExpression _ expr op)
       `(postfix ,(Expression->sexp expr) ,op)]
      [($ InfixExpression _ left op right)
       `(,op ,(Expression->sexp left) ,(Expression->sexp right))]
      [($ ConditionalExpression _ test consequent alternate)
       `(? ,(Expression->sexp test)
           ,(Expression->sexp consequent)
           ,(Expression->sexp alternate))]
      [($ AssignmentExpression _ lhs op rhs)
       `(,op ,(Expression->sexp lhs) ,(Expression->sexp rhs))]
      [($ FunctionExpression _ #f (($ Identifier _ args) ...) body)
       `(function ,args ,@(map SourceElement->sexp body))]
      [($ FunctionExpression _ ($ Identifier _ name) (($ Identifier _ args) ...) body)
       `(function ,name ,args ,@(map SourceElement->sexp body))]
      [($ ListExpression _ exprs)
       `(begin ,@(map Expression->sexp exprs))]
      [($ CallExpression _ method args)
       `(,(Expression->sexp method) ,@(map Expression->sexp args))]
      [($ ParenExpression _ expression)
       (Expression->sexp expression)]))

  ;; SourceElement->sexp : SourceElement -> sexp
  (define (SourceElement->sexp elt)
    (match elt
      [(? FunctionDeclaration?)
       (FunctionDeclaration->sexp elt)]
      [($ VariableDeclaration _ inits)
       `(var ,@(map VariableInitializer->sexp inits))]
      [_ (Statement->sexp elt)]))

  ;; Property->sexp : Property -> sexp
  (define (Property->sexp elt)
    (match elt
      [($ Identifier _ name) name]
      [($ StringLiteral _ value) value]
      [($ NumericLiteral _ value) value]))

  ;; ArrayElement->sexp : (optional Expression) -> sexp
  (define (ArrayElement->sexp elt)
    (if elt (Expression->sexp elt) '()))

  ;; Statement->sexp : Statement -> sexp
  (define (Statement->sexp stmt)
    (match stmt
      [($ BlockStatement _ elts)
       `(block ,@(map SubStatement->sexp elts))]
      [($ EmptyStatement _)
       '()]
      [($ ExpressionStatement _ expr)
       (Expression->sexp expr)]
      [($ IfStatement _ test consequent alternate)
       (if alternate
           `(if ,(Expression->sexp test)
                ,(SubStatement->sexp consequent)
                ,(SubStatement->sexp alternate))
           `(if ,(Expression->sexp test)
                ,(SubStatement->sexp consequent)))]
      [($ DoWhileStatement _ body test)
       `(do ,(SubStatement->sexp body)
          ,(Expression->sexp test))]
      [($ WhileStatement _ test body)
       `(while ,(Expression->sexp test)
          ,(SubStatement->sexp body))]
      [($ ForStatement _ init test incr body)
       `(for ,(cond
                [(not init) #f]
                [(VariableDeclaration? init) (SourceElement->sexp init)]
                [else (Expression->sexp init)])
             ,(if test (Expression->sexp test) #t)
             ,(if incr (Expression->sexp incr) #f)
             ,(SubStatement->sexp body))]
      [($ ForInStatement _ ($ VariableDeclaration _ (($ VariableInitializer _ ($ Identifier _ var) #f))) container body)
       `(for-in ((var ,var) ,(Expression->sexp container))
          ,(SubStatement->sexp body))]
      [($ ForInStatement _ (and var (? Expression?)) container body)
       `(for-in (,(Expression->sexp var) ,(Expression->sexp container))
          ,(SubStatement->sexp body))]
      [($ ContinueStatement _ #f)
       '(continue)]
      [($ ContinueStatement _ ($ Identifier _ id))
       `(continue ,id)]
      [($ BreakStatement _ #f)
       '(break)]
      [($ BreakStatement _ ($ Identifier _ id))
       `(break ,id)]
      [($ ReturnStatement _ value)
       (if value `(return ,(Expression->sexp value)) '(return))]
      [($ WithStatement _ context body)
       `(with ,(Expression->sexp context) ,(SubStatement->sexp body))]
      [($ SwitchStatement _ test cases)
       `(switch ,(Expression->sexp test)
          ,@(map CaseClause->sexp cases))]
      [($ LabelledStatement _ ($ Identifier _ label) stmt)
       `(label ,label ,(SubStatement->sexp stmt))]
      [($ ThrowStatement _ value)
       `(throw ,(Expression->sexp value))]
      [($ TryStatement _ body catch finally)
       `(try ,(Statement->sexp body)
          ,@(map CatchClause->sexp catch)
          ,@(if finally (list `(finally ,(Statement->sexp finally))) null))]))

  ;; SubStatement->sexp : SubStatement -> sexp
  (define (SubStatement->sexp elt)
    (SourceElement->sexp elt))

  ;; FunctionDeclaration->sexp : FunctionDeclaration -> sexp
  (define (FunctionDeclaration->sexp elt)
    (match elt
      [($ FunctionDeclaration _ ($ Identifier _ name) (($ Identifier _ args) ...) body)
       `(function ,name ,args ,@(map SourceElement->sexp body))]))

  ;; VariableInitializer->sexp : VariableInitializer -> sexp
  (define (VariableInitializer->sexp decl)
    (match decl
      [($ VariableInitializer _ ($ Identifier _ id) #f)
       id]
      [($ VariableInitializer _ ($ Identifier _ id) init)
       `[,id ,(Expression->sexp init)]]))

  ;; CaseClause->sexp : CaseClause -> sexp
  (define (CaseClause->sexp clause)
    (match clause
      [($ CaseClause _ #f stmts)
       `(default ,@(map SubStatement->sexp stmts))]
      [($ CaseClause _ expr stmts)
       `(case ,(Expression->sexp expr) ,@(map SubStatement->sexp stmts))]))

  ;; CatchClause->sexp : CatchClause -> sexp
  (define (CatchClause->sexp clause)
    (match clause
      [($ CatchClause _ ($ Identifier _ id) body)
       `(catch ,id ,(Statement->sexp body))]))

  (provide/contract
    [sexp? predicate/c]
    [Expression->sexp (Expression? . -> . sexp?)]
    [Statement->sexp (Statement? . -> . sexp?)]
    [SourceElement->sexp (SourceElement? . -> . sexp?)]
    [sexp->Expression (sexp? . -> . Expression?)]
    [sexp->Statement (sexp? . -> . Statement?)]
    [sexp->SourceElement (sexp? . -> . SourceElement?)]))
