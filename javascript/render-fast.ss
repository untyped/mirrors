#lang scheme/base

(require scheme/match
         "struct.ss")

; Procedures -------------------------------------

; javascript -> string
(define (fast-javascript->string js)
  (let ([out (open-output-string)])
    (display-javascript js out)
    (get-output-string out)))

; javascript output-port -> string
(define (display-javascript js out)
  (define (display-str str)
    (display str out))
  (define display-js
    (match-lambda
      [(struct BeginStatement (_ stmts))               (for-each display-js stmts)]
      [(struct Identifier (_ name))                    (display-str name)]
      [(struct CaseClause (_ expr stmts))              (if expr
                                                           (begin (display-str "case ")
                                                                  (display-js js)
                                                                  (display-str ": ")
                                                                  (for-each display-js stmts))
                                                           (begin (display-str "default: ")
                                                                  (for-each display-js stmts)))]
      [(struct CatchClause (_ id body))                (display-str "catch (")
                                                       (display-js id)
                                                       (display-str ") ")
                                                       (display-js body)]
      [(struct VariableInitializer (_ id expr))        (display-js id)
                                                       (when expr
                                                         (display-str " = ")
                                                         (display-js expr))]
      [(struct FunctionDeclaration (_ id args body))   (display-str "function")
                                                       (display-js id)
                                                       (display-str "(")
                                                       (display-list ", " args)
                                                       (display-str ") { ")
                                                       (for-each display-js body)
                                                       (display-str "}")]
      [(struct VariableDeclaration (_ bindings))       (display-str "var ")
                                                       (display-list ", " bindings)
                                                       (display-str "; ")]
      [(struct StringLiteral (_ value))                (write value out)]
      [(struct RegexpLiteral (_ pattern g? ci?))       (display-str "/")
                                                       (display-str pattern)
                                                       (display-str "/")
                                                       (when g? (display-str "g"))
                                                       (when ci? (display-str "i"))]
      [(struct NumericLiteral (_ value))               (display-str value)]
      [(struct BooleanLiteral (_ value))               (display-str (if value "true" "false"))]
      [(struct NullLiteral (_))                        (display-str "null")]
      [(struct ArrayLiteral (_ elems))                 (display-str "[ ")
                                                       (display-list ", " elems)
                                                       (display-str " ]")]
      [(struct ObjectLiteral (_ props))                (display-str "{ ")
                                                       (display-property-list props)
                                                       (display-str " }")]
      [(struct ThisReference (_))                      (display-str "this")]
      [(struct VarReference (_ id))                    (display-js id)]
      [(struct BracketReference (_ container key))     (display-js container)
                                                       (display-str "[")
                                                       (display-js key)
                                                       (display-str "]")]
      [(struct DotReference (_ container id))          (display-js container)
                                                       (display-str ".")
                                                       (display-js id)]
      [(struct RawExpression (_ str))                  (display str out)]
      [(struct NewExpression (_ id args))              (display-str "new ")
                                                       (display-js id)
                                                       (display-str "(")
                                                       (display-list ", " args)
                                                       (display-str ")")]
      [(struct PostfixExpression (_ expr op))          (display-js expr)
                                                       (display-str op)]
      [(struct PrefixExpression (_ op expr))           (display-str op)
                                                       (display-js expr)]
      [(struct InfixExpression (_ left op right))      (display-str "(")
                                                       (display-js left)
                                                       (display-str " ")
                                                       (display-str op)
                                                       (display-str " ")
                                                       (display-js right)
                                                       (display-str ")")]
      [(struct ConditionalExpression (_ test pos neg)) (display-str "(")
                                                       (display-js test)
                                                       (display-str " ? ")
                                                       (display-js pos)
                                                       (display-str " : ")
                                                       (display-js neg)
                                                       (display-str ")")]
      [(struct AssignmentExpression (_ lhs op rhs))    (display-str "(")
                                                       (display-js lhs)
                                                       (display-str " ")
                                                       (display-str op)
                                                       (display-str " ")
                                                       (display-js rhs)
                                                       (display-str ")")]
      [(struct FunctionExpression (_ id args body))    (display-str "(function")
                                                       (when id
                                                         (display-js id)
                                                         (display-str " "))
                                                       (display-str "(")
                                                       (display-list ", " args)
                                                       (display-str ") { ")
                                                       (for-each display-js body)
                                                       (display-str "})")]
      [(struct CallExpression (_ fn args))             (display-js fn)
                                                       (display-str "(")
                                                       (display-list ", " args)
                                                       (display-str ")")]
      [(struct ParenExpression (_ expr))               (display "(" out)
                                                       (display-js expr)
                                                       (display ")" out)]
      [(struct ListExpression (_ exprs))               (display-str "(")
                                                       (display-list ", " exprs)
                                                       (display-str ")")]
      [(struct BlockStatement (_ stmts))               (display-str "{ ")
                                                       (for-each display-js stmts)
                                                       (display-str " }")]
      [(struct EmptyStatement (_))                     (display-str "; ")]
      [(struct ExpressionStatement (_ expr))           (display-js expr)
                                                       (display-str "; ")]
      [(struct IfStatement (_ test pos neg))           (display-str "if(")
                                                       (display-js test)
                                                       (display-str ") ")
                                                       (display-js pos)
                                                       (when neg
                                                         (display-str "else ")
                                                         (display-js neg))]
      [(struct DoWhileStatement (_ body expr))         (display-str "do ")
                                                       (display-js body)
                                                       (display-str " while(")
                                                       (display-js expr)
                                                       (display-str ");")]
      [(struct WhileStatement (_ expr body))           (display-str "while(")
                                                       (display-js expr)
                                                       (display-str ") ")
                                                       (display-js body)]
      [(struct ForStatement (_ init test incr body))   (display-str "for(")
                                                       (when init (display-js init))
                                                       (display-str "; ")
                                                       (when test (display-js test))
                                                       (display-str "; ")
                                                       (when incr (display-js incr))
                                                       (display-str ") ")
                                                       (display-js body)]
      [(struct ForInStatement (_ item container body)) (display-str "for(")
                                                       (display-js item)
                                                       (display-str " in ")
                                                       (display-js container)
                                                       (display-str ") ")
                                                       (display-js body)]
      [(struct ContinueStatement (_ label))            (display-str "continue")
                                                       (when label
                                                         (display-str " ")
                                                         (display-js label))
                                                       (display-str "; ")]
      [(struct BreakStatement (_ label))               (display-str "break")
                                                       (when label
                                                         (display-str " ")
                                                         (display-js label))
                                                       (display-str "; ")]
      [(struct ReturnStatement (_ expr))               (display-str "return")
                                                       (when expr
                                                         (display-str " ")
                                                         (display-js expr))
                                                       (display-str "; ")]
      [(struct WithStatement (_ expr body))            (display-str "with(")
                                                       (display-js expr)
                                                       (display-str ") ")
                                                       (display-js body)]
      [(struct SwitchStatement (_ expr cases))         (display-str "switch(")
                                                       (display-js expr)
                                                       (display-str ") { ")
                                                       (for-each display-js cases)
                                                       (display-str " }")]
      [(struct LabelledStatement (_ label stmt))       (display-js label)
                                                       (display-str ": ")
                                                       (display-js stmt)]
      [(struct ThrowStatement (_ expr))                (display-str "throw ")
                                                       (display-js expr)
                                                       (display-str "; ")]
      [(struct TryStatement (_ body catches finally))  (display-str "try ")
                                                       (display-js body)
                                                       (for-each display-js catches)
                                                       (when finally
                                                         (display-str " finally ")
                                                         (display-js finally))]))
  (define (display-list delimiter items)
    (match items
      [(list)                (void)]
      [(list curr)           (display-js curr)]
      [(list-rest curr rest) (display-js curr)
                             (display-str delimiter)
                             (display-list delimiter rest)]))
  (define (display-property-list props)
    (match props
      [(list) (void)]
      [(list (list-rest prop expr))
       (display-js prop)
       (display-str ": ")
       (display-js expr)]
      [(list-rest (list-rest prop expr) rest)
       (display-js prop)
       (display-str ": ")
       (display-js expr)
       (display-str ", ")
       (display-property-list rest)]))
  (display-js js))

; Provide statements -----------------------------

(provide fast-javascript->string)
