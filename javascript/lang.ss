#lang scheme/base

(require scheme/contract
         (only-in srfi/1/list 
                  drop-right
                  take-right)
         (file "../javascript.plt-5.4-hacked/syntax/ast.ss")
         (file "../base.ss")
         (file "op-util.ss")
         (file "quote.ss"))

; API --------------------------------------------

; (U id+quotable var-init) (U id+quotable var-init) ... -> var-decl
(define (js:var . initializers)
  (make-VariableDeclaration #f (map (lambda (arg)
                                      (if (VariableInitializer? arg)
                                          arg
                                          (make-VariableInitializer #f (quote-identifier arg) #f)))
                                    initializers)))

; id+quotable [expr+quotable] -> var-init
(define (js:init id [expr (void)])
  (make-VariableInitializer #f (quote-identifier id) (if (void? expr) #f (quote-expression expr))))

; -> empty-stmt
(define (js:empty)
  (make-EmptyStatement #f))

; stmt+quotable ... -> block-stmt
(define (js:begin . stmts)
  (wrap-begin stmts))

; stmt+quotable ... -> block-stmt
(define (js:block . stmts)
  (make-BlockStatement #f (map quote-javascript stmts)))

(define js:if
  (case-lambda
    [(test positive)
     (make-IfStatement #f (quote-expression test) (wrap-block positive) #f)]
    [(test positive negative)
     (make-IfStatement #f (quote-expression test) (wrap-block (list positive)) (wrap-block (list negative)))]))

; stmt+quotable ... #:while expr -> do-stmt
(define (js:do #:while expr . stmts)
  (make-DoWhileStatement #f (wrap-block stmts) (quote-expression expr)))

; expr+quotable stmt+quotable ... -> while-stmt
(define (js:while expr . stmts)
  (make-WhileStatement #f (quote-expression expr) (wrap-block stmts)))

; [#:init var-decl] [#:test expr+quotable] [#:incr expr+quotable] stmt ... -> for-stmt
(define (js:for #:init [init (void)] #:test [test (void)] #:incr [incr (void)] . stmts)
  (make-ForStatement #f 
                     (if (void? init) #f init)
                     (if (void? test) #f (quote-expression test))
                     (if (void? incr) #f (quote-expression incr))
                     (wrap-block stmts)))

; var-decl expr+quotable . stmt+quotable ... -> for-in-stmt
(define (js:for-in var container . stmts)
  (make-ForInStatement #f
                       (cond [(VariableDeclaration? var) var]
                             [(Expression? var)          var]
                             [(pair? var)                (quote-identifier (car var))]
                             [else                       (quote-identifier var)])
                       (quote-expression container)
                       (wrap-block stmts)))

; [id+quotable] -> break-stmt
(define (js:break [label (void)])
  (if (void? label)
      (make-BreakStatement #f #f)
      (make-BreakStatement #f (quote-identifier label))))

; [id+quotable] -> continue-stmt
(define (js:continue [label (void)])
  (if (void? label)
      (make-ContinueStatement #f #f)
      (make-ContinueStatement #f (quote-identifier label))))

; [expr+quotable] -> return-stmt
(define (js:return [label (void)])
  (if (void? label)
      (make-ReturnStatement #f #f)
      (make-ReturnStatement #f (quote-expression label))))

; expr+quotable stmt+quotable ... -> with-stmt
(define (js:with expr . stmts)
  (make-WithStatement #f (quote-expression expr) (wrap-block stmts)))

; expr+quotable switch-clause ... -> switch-stmt
(define (js:switch expr . clauses)
  (make-SwitchStatement #f (quote-expression expr) clauses))

; expr+quotable stmt+quotable ... -> switch-clause
(define (js:case expr . stmts)
  (make-CaseClause #f (quote-expression expr) (wrap-block stmts)))

; stmt+quotable ... -> switch-clause
(define (js:default . stmts)
  (make-CaseClause #f #f (wrap-block stmts)))

; id+quotable stmt+quotable -> label-stmt
(define (js:label id stmt)
  (make-LabelledStatement #f (quote-identifier id) (quote-statement stmt)))

; expr+quotable -> throw-stmt
(define (js:throw expr)
  (make-ThrowStatement #f (quote-expression expr)))

; (U stmt+quotable catch-clause) ... -> try-stmt
(define (js:try . stmts+clauses)
  (make-TryStatement #f
                     (wrap-block (filter statement+quotable? stmts+clauses))
                     (filter CatchClause? stmts+clauses)))

; id+quotable stmt+quotable ... -> catch-clause
(define (js:catch id . stmts)
  (make-CatchClause #f (quote-identifier id) (wrap-block stmts)))

; stmt+quotable ... -> catch-clause
(define (js:finally . stmts)
  (make-CatchClause #f #f (wrap-block stmts)))

; expr+quotable ... -> array-expr
(define (js:array . exprs)
  (make-ArrayLiteral #f (map quote-expression exprs)))

; field ... -> object-expr
(define (js:object . fields)
  (make-ObjectLiteral #f fields))

; (U Identifier StringLiteral NumericLiteral symbol string integer) expr+quotable -> (cons Property Expression)
(define (js:field index expr)
  (cons (cond [(Identifier? index)     index]
              [(StringLiteral? index)  index]
              [(NumericLiteral? index) index]
              [(symbol? index)         (make-Identifier #f index)]
              [(string? index)         (make-StringLiteral #f index)]
              [(integer? index)        (make-NumericLiteral #f index)])
        (quote-expression expr)))

; expr+quotable [#:global? boolean] [#:ci? boolean] -> RegexpLiteral
(define (js:regexp value #:global? [global? #f] #:ci? [ci? #f])
  (make-RegexpLiteral #f value global? ci?))

; expr+quotable expr+quotable -> index-expr
(define (js:index expr index)
  (make-BracketReference #f (quote-expression expr) (quote-expression index)))

; expr+quotable ... -> (U dot-expr expr)
(define (js:dot . args)
  (cond [(null? args)        (error (format "js:dot: expected one or more arguments, received none."))]
        [(null? (cdr args))  (error (format "js:dot: expected one or more arguments, received one: ~a." (car args)))]
        [(null? (cddr args)) (make-DotReference #f (quote-expression (car args)) (quote-identifier (cadr args)))]
        [else                (let ([most (drop-right args 1)]
                                   [last (car (take-right args 1))])
                               (make-DotReference #f (apply js:dot most) (quote-identifier last)))]))

; expr+quotable ... -> new-expr
(define (js:new constructor . args)
  (make-NewExpression #f (quote-expression constructor) (map quote-expression args)))

; expr+quotable -> expr
(define-prefix-expr-constructors)

; expr+quotable -> expr
(define-postfix-expr-constructors)

; expr+quotable ... -> expr
(define-infix-expr-constructors)

; expr+quotable expr+quotable -> expr
(define-assignment-expr-constructors)

; (listof id_quotable) expr+quotable ... -> call-expr
(define (js:function args . stmts)
  (make-FunctionExpression #f #f (map quote-identifier args) (map quote-javascript stmts)))

; expr+quotable ... -> call-expr
(define (js:all . args)
  (make-ListExpression #f (map quote-expression args)))

; expr+quotable ... -> call-expr
(define (js:call function . args)
  (make-CallExpression #f (parenthesize-anonymous-function (quote-expression function)) (map quote-expression args)))

; symbol -> id
(define (js:id sym)
  (make-Identifier #f sym))

; Provide statements -----------------------------

(provide/contract
 [rename js:var      var      (->* ((or/c identifier+quotable? VariableInitializer?)) () #:rest (listof (or/c identifier+quotable? VariableInitializer?)) VariableDeclaration?)]
 [rename js:init     init     (->* (identifier+quotable?) (expression+quotable?) VariableInitializer?)]
 [rename js:begin    begin    (->* () () #:rest (listof javascript+quotable?) SourceElement?)]
 [rename js:block    block    (->* () () #:rest (listof javascript+quotable?) SourceElement?)]
 [rename js:if       if       (->* (expression+quotable? statement+quotable?) (statement+quotable?) IfStatement?)]
 [rename js:do       do       (->* (#:while expression+quotable?) () #:rest (listof statement+quotable?) DoWhileStatement?)]
 [rename js:while    while    (->* (expression+quotable?) () #:rest (listof statement+quotable?) WhileStatement?)]
 [rename js:for      for      (->* () (#:init (or/c VariableDeclaration? expression+quotable?) #:test expression+quotable? #:incr expression+quotable?) #:rest (listof statement+quotable?) ForStatement?)]
 [rename js:for-in   for-in   (->* ((or/c VariableDeclaration? expression+quotable?) expression+quotable?) () #:rest (listof statement+quotable?) ForInStatement?)]
 [rename js:break    break    (->* () (identifier+quotable?) BreakStatement?)]
 [rename js:continue continue (->* () (identifier+quotable?) ContinueStatement?)]
 [rename js:return   return   (->* () (expression+quotable?) ReturnStatement?)]
 [rename js:with     with     (->* (expression+quotable?) () #:rest (listof statement+quotable?) WithStatement?)]
 [rename js:switch   switch   (->* (expression+quotable?) () #:rest (listof CaseClause?) SwitchStatement?)]
 [rename js:case     case     (->* (expression+quotable?) () #:rest (listof statement+quotable?) CaseClause?)]
 [rename js:default  default  (->* () () #:rest (listof statement+quotable?) CaseClause?)]
 [rename js:label    label    (->* (identifier+quotable? statement+quotable?) () LabelledStatement?)]
 [rename js:throw    throw    (->* (expression+quotable?) () ThrowStatement?)]
 [rename js:try      try      (->* () () #:rest (listof (or/c statement+quotable? CatchClause?)) TryStatement?)]
 [rename js:catch    catch    (->* (identifier+quotable?) () #:rest (listof statement+quotable?) CatchClause?)]
 [rename js:finally  finally  (->* () () #:rest (listof statement+quotable?) CatchClause?)]
 [rename js:array    array    (->* () () #:rest (listof expression+quotable?) ArrayLiteral?)]
 [rename js:object   object   (->* () () #:rest (listof (cons/c (or/c Identifier? StringLiteral? NumericLiteral? symbol? string? integer?) expression+quotable?)) ObjectLiteral?)]
 [rename js:index    index    (->* (expression+quotable? expression+quotable?) () BracketReference?)]
 [rename js:regexp   regexp   (->* (string?) (#:global? boolean? #:ci? boolean?) RegexpLiteral?)]
 [rename js:dot      dot      (->* (expression+quotable?) () #:rest (listof expression+quotable?) DotReference?)]
 [rename js:new      new      (->* (expression+quotable?) () #:rest (listof expression+quotable?) NewExpression?)]
 [rename js:function function (->* ((listof identifier+quotable?)) () #:rest (listof javascript+quotable?) FunctionExpression?)]
 [rename js:all      all      (->* () () #:rest (listof expression+quotable?) ListExpression?)]
 [rename js:call     call     (->* (expression+quotable?) () #:rest (listof expression+quotable?) CallExpression?)]
 [rename js:id       id       (->* (symbol?) () Identifier?)])
