(module ast-checked mzscheme
  (require (planet "contract-utils.ss" ("cobbe" "contract-utils.plt" 1 0))
           (lib "contract.ss")
           "ast-core.ss"
           "ast-utils.ss"
           "token.ss")

  (provide/contract
    [Term? predicate/c]
    [Term-location (Term? . -> . (optional/c region?))])

  (provide/contract
    (struct (Declaration Term) ([location (optional/c region?)]))
    (struct (Expression Term) ([location (optional/c region?)]))
    (struct (Statement Term) ([location (optional/c region?)]))
    (struct (Identifier Term) ([location (optional/c region?)]
                               [name symbol?]))
    (struct (CaseClause Term) ([location (optional/c region?)]
                               [question (optional/c Expression/X?)]
                               [answer SubStatementList/X?]))
    (struct (CatchClause Term) ([location (optional/c region?)]
                                [id Identifier?]
                                [body Statement/X?]))
    (struct (VariableInitializer Term) ([location (optional/c region?)]
                                        [id Identifier?]
                                        [init (optional/c Expression/X?)])))

  (provide/contract
    (struct (FunctionDeclaration Declaration) ([location (optional/c region?)]
                                               [name Identifier?]
                                               [args (listof Identifier?)]
                                               [body (listof SourceElement?)]))
    (struct (VariableDeclaration Declaration) ([location (optional/c region?)]
                                               [bindings (nelistof/c VariableInitializer?)]))
    (struct (LetDeclaration Declaration) ([location (optional/c region?)]
                                          [bindings (union (nelistof/c VariableInitializer?)
                                                           (nelistof/c FunctionDeclaration?))])))

  (provide/contract
    (struct (StringLiteral Expression) ([location (optional/c region?)]
                                        [value string?]))
    (struct (RegexpLiteral Expression) ([location (optional/c region?)]
                                        [pattern string?]
                                        [global? boolean?]
                                        [case-insensitive? boolean?]))
    (struct (NumericLiteral Expression) ([location (optional/c region?)]
                                         [value number?]))
    (struct (BooleanLiteral Expression) ([location (optional/c region?)]
                                         [value boolean?]))
    (struct (NullLiteral Expression) ([location (optional/c region?)]))
    (struct (ArrayLiteral Expression) ([location (optional/c region?)]
                                       [elements (listof (optional/c Expression/X?))]))
    (struct (ObjectLiteral Expression) ([location (optional/c region?)]
                                        [properties (listof (cons/c Property? Expression/X?))]))
    (struct (ThisReference Expression) ([location (optional/c region?)]))
    (struct (VarReference Expression) ([location (optional/c region?)]
                                       [id Identifier?]))
    (struct (BracketReference Expression) ([location (optional/c region?)]
                                           [container Expression/X?]
                                           [key Expression/X?]))
    (struct (DotReference Expression) ([location (optional/c region?)]
                                       [container Expression/X?]
                                       [id Identifier?]))
    (struct (NewExpression Expression) ([location (optional/c region?)]
                                        [constructor Expression/X?]
                                        [arguments ExpressionList/X?]))
    (struct (PostfixExpression Expression) ([location (optional/c region?)]
                                            [expression Expression/X?]
                                            [operator PostfixOperator/c]))
    (struct (PrefixExpression Expression) ([location (optional/c region?)]
                                           [operator PrefixOperator/c]
                                           [expression Expression/X?]))
    (struct (InfixExpression Expression) ([location (optional/c region?)]
                                          [left Expression/X?]
                                          [operator InfixOperator/c]
                                          [right Expression/X?]))
    (struct (ConditionalExpression Expression) ([location (optional/c region?)]
                                                [test Expression/X?]
                                                [consequent Expression/X?]
                                                [alternate Expression/X?]))
    (struct (AssignmentExpression Expression) ([location (optional/c region?)]
                                               [lhs Expression/X?]
                                               [operator AssignmentOperator/c]
                                               [rhs Expression/X?]))
    (struct (FunctionExpression Expression) ([location (optional/c region?)]
                                             [name (optional/c Identifier?)]
                                             [args (listof Identifier?)]
                                             [body any/c #;(listof SourceElement?)]))
    (struct (LetExpression Expression) ([location (optional/c region?)]
                                        [bindings (listof VariableInitializer?)]
                                        [body Expression/X?]))
    (struct (CallExpression Expression) ([location (optional/c region?)]
                                         [method Expression/X?]
                                         [args ExpressionList/X?]))
    (struct (ParenExpression Expression) ([location (optional/c region?)]
                                          [expression Expression/X?]))
    (struct (ListExpression Expression) ([location (optional/c region?)]
                                         [expressions ExpressionList/X?])))

  (provide/contract
    (struct (BlockStatement Statement) ([location (optional/c region?)]
                                        [statements SubStatementList/X?]))
    (struct (EmptyStatement Statement) ([location (optional/c region?)]))
    (struct (ExpressionStatement Statement) ([location (optional/c region?)]
                                             [expression Expression/X?]))
    (struct (IfStatement Statement) ([location (optional/c region?)]
                                     [test Expression/X?]
                                     [consequent SubStatement/X?]
                                     [alternate (optional/c SubStatement/X?)]))
    (struct (DoWhileStatement Statement) ([location (optional/c region?)]
                                          [body SubStatement/X?]
                                          [test Expression/X?]))
    (struct (WhileStatement Statement) ([location (optional/c region?)]
                                        [test Expression/X?]
                                        [body SubStatement/X?]))
    (struct (ForStatement Statement) ([location (optional/c region?)]
                                      [init (union (optional/c Expression/X?) VariableDeclaration?)]
                                      [test (optional/c Expression/X?)]
                                      [incr (optional/c Expression/X?)]
                                      [body SubStatement/X?]))
    (struct (ForInStatement Statement) ([location (optional/c region?)]
                                        [lhs (union Expression/X? VariableDeclaration?)]
                                        [container Expression/X?]
                                        [body SubStatement/X?]))
    (struct (ContinueStatement Statement) ([location (optional/c region?)]
                                           [label (optional/c Identifier?)]))
    (struct (BreakStatement Statement) ([location (optional/c region?)]
                                        [label (optional/c Identifier?)]))
    (struct (ReturnStatement Statement) ([location (optional/c region?)]
                                         [value (optional/c Expression/X?)]))
    (struct (LetStatement Statement) ([location (optional/c region?)]
                                      [bindings (listof VariableInitializer?)]
                                      [body SubStatement/X?]))
    (struct (WithStatement Statement) ([location (optional/c region?)]
                                       [context Expression/X?]
                                       [body SubStatement/X?]))
    (struct (SwitchStatement Statement) ([location (optional/c region?)]
                                         [expression Expression/X?]
                                         [cases (listof CaseClause?)]))
    (struct (LabelledStatement Statement) ([location (optional/c region?)]
                                           [label Identifier?]
                                           [statement SubStatement/X?]))
    (struct (ThrowStatement Statement) ([location (optional/c region?)]
                                        [value Expression/X?]))
    (struct (TryStatement Statement) ([location (optional/c region?)]
                                      [body Statement/X?]
                                      [catch (listof CatchClause?)]
                                      [finally (optional/c Statement/X?)])))

  (provide (all-from "ast-utils.ss")))
