(module ast-core mzscheme
  (require (planet "contract-utils.ss" ("cobbe" "contract-utils.plt" 1 0))
           (lib "contract.ss"))

  ;; TODO: save location information for operators?

  ;; ===========================================================================
  ;; TERMS
  ;; ===========================================================================

  (define-struct Term (location) #f)

  (define-struct (Declaration Term) () #f)
  (define-struct (Statement Term) () #f)
  (define-struct (Expression Term) () #f)

  ;; ===========================================================================
  ;; DECLARATIONS
  ;; ===========================================================================

  ;; Identifier * (listof Identifier) * (listof SourceElement)
  (define-struct (FunctionDeclaration Declaration) (name args body) #f)

  ;; (nelistof VariableInitializer)
  (define-struct (VariableDeclaration Declaration) (bindings) #f)

  ;; Identifier * (optional Expression)
  (define-struct (VariableInitializer Term) (id init) #f)

  ;; TODO: should `let-ness' be a property of FunctionDeclarations, rather than
  ;;       wrapping FunctionDeclarations inside LetDeclarations?
  ;; (union (nelistof VariableInitializer) (nelistof FunctionDeclaration))
  (define-struct (LetDeclaration Declaration) (bindings) #f)

  ;; ===========================================================================
  ;; EXPRESSIONS
  ;; ===========================================================================

  ;; string
  (define-struct (StringLiteral Expression) (value) #f)

  ;; number
  (define-struct (NumericLiteral Expression) (value) #f)

  ;; boolean
  (define-struct (BooleanLiteral Expression) (value) #f)

  ;;
  (define-struct (NullLiteral Expression) () #f)

  ;; string * boolean * boolean
  (define-struct (RegexpLiteral Expression) (pattern global? case-insensitive?) #f)

  ;; (listof (optional Expression))
  (define-struct (ArrayLiteral Expression) (elements) #f)

  ;; (listof (cons Property Expression))
  (define-struct (ObjectLiteral Expression) (properties) #f)

  ;; 
  (define-struct (ThisReference Expression) () #f)

  ;; Identifier
  (define-struct (VarReference Expression) (id) #f)

  ;; Expression * Expression
  (define-struct (BracketReference Expression) (container key) #f)

  ;; Expression * Identifier
  (define-struct (DotReference Expression) (container id) #f)

  ;; Expression * (listof Expression)
  (define-struct (NewExpression Expression) (constructor arguments) #f)

  ;; Expression * PostfixOperator
  (define-struct (PostfixExpression Expression) (expression operator) #f)

  ;; PrefixOperator * Expression
  (define-struct (PrefixExpression Expression) (operator expression) #f)

  ;; Expression * InfixOperator * Expression
  (define-struct (InfixExpression Expression) (left operator right) #f)

  ;; Expression * Expression * Expression
  (define-struct (ConditionalExpression Expression) (test consequent alternate) #f)

  ;; Expression * AssignmentOperator * Expression
  (define-struct (AssignmentExpression Expression) (lhs operator rhs) #f)

  ;; (optional Identifier) * (listof Identifier) * (listof SourceElement)
  (define-struct (FunctionExpression Expression) (name args body) #f)

  ;; (listof VariableInitializer) Expression
  (define-struct (LetExpression Expression) (bindings body) #f)

  ;; Expression * (listof Expression)
  (define-struct (CallExpression Expression) (method args) #f)

  ;; Expression
  (define-struct (ParenExpression Expression) (expression) #f)

  ;; (listof Expression)
  (define-struct (ListExpression Expression) (expressions) #f)

  ;; ===========================================================================
  ;; STATEMENTS
  ;; ===========================================================================

  ;; (listof SubStatement)
  (define-struct (BlockStatement Statement) (statements) #f)

  ;; 
  (define-struct (EmptyStatement Statement) () #f)

  ;; Expression
  (define-struct (ExpressionStatement Statement) (expression) #f)

  ;; Expression * SubStatement * (optional SubStatement)
  (define-struct (IfStatement Statement) (test consequent alternate) #f)

  ;; SubStatement * Expression
  (define-struct (DoWhileStatement Statement) (body test) #f)

  ;; Expression * SubStatement
  (define-struct (WhileStatement Statement) (test body) #f)

  ;; (union (optional Expression) VariableDeclaration) * (optional Expression) * (optional Expression) * SubStatement
  (define-struct (ForStatement Statement) (init test incr body) #f)

  ;; (union Expression VariableDeclaration) * Expression * SubStatement
  (define-struct (ForInStatement Statement) (lhs container body) #f)

  ;; (optional Identifier)
  (define-struct (ContinueStatement Statement) (label) #f)

  ;; (optional Identifier)
  (define-struct (BreakStatement Statement) (label) #f)

  ;; (optional Expression)
  (define-struct (ReturnStatement Statement) (value) #f)

  ;; (listof VariableInitializer) SubStatement
  (define-struct (LetStatement Statement) (bindings body) #f)

  ;; Expression * SubStatement
  (define-struct (WithStatement Statement) (context body) #f)

  ;; Expression * (listof CaseClause)
  (define-struct (SwitchStatement Statement) (expression cases) #f)

  ;; Identifier * SubStatement
  (define-struct (LabelledStatement Statement) (label statement) #f)

  ;; Expression
  (define-struct (ThrowStatement Statement) (value) #f)

  ;; Statement * (listof CatchClause) * (optional Statement)
  (define-struct (TryStatement Statement) (body catch finally) #f)

  ;; symbol
  (define-struct (Identifier Term) (name) #f)

  ;; (optional Expression) * (listof SubStatement)
  (define-struct (CaseClause Term) (question answer) #f)

  ;; Identifier * Statement
  (define-struct (CatchClause Term) (id body) #f)

  (provide (all-defined)))