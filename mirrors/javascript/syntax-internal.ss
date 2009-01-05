#lang scheme/base

(require (planet untyped/unlib/syntax)
         (planet untyped/unlib/symbol)
         (file "../base.ss")
         (file "op.ss")
         (file "struct.ss")
         (for-template scheme/base
                       (prefix-in js: (file "lang.ss"))
                       (file "quote.ss")
                       (file "struct.ss")))

; syntax [boolean] -> syntax
(define (expand-javascript stx [quote-expression? #t])
  (syntax-case* stx (var function) symbolic-identifier=?
    [(var init ...)                   (expand-declaration #'(var init ...))]
    [(function id (arg ...) stmt ...) (expand-declaration #'(function id (arg ...) stmt ...))]
    [stmt                             (expand-statement #'stmt quote-expression?)]))

; syntax -> syntax
(define (expand-declaration stx)
  (syntax-case* stx (var function) symbolic-identifier=?
    [(var init ...)                   #`(make-VariableDeclaration #f (list #,@(map expand-initializer (syntax->list #'(init ...)))))]
    [(function id (arg ...) stmt ...) (identifier? #'id)
                                      #`(make-FunctionDeclaration 
                                         #f
                                         #,(expand-identifier #'id) 
                                         (list #,@(map expand-identifier (syntax->list #'(arg ...))))
                                         (list #,@(map expand-javascript (syntax->list #'(stmt ...)))))]
    ; HACK : Gets around a syntax error when trying to define anonymous functions at the top level of a "js" form:
    [(function (arg ...) stmt ...)    #`(make-FunctionExpression
                                         #f
                                         #f
                                         (list #,@(map expand-identifier (syntax->list #'(arg ...))))
                                         (list #,@(map expand-javascript (syntax->list #'(stmt ...)))))]
    [(function any ...)               (raise-syntax-error #f "bad syntax" (syntax->datum stx))]))

; syntax -> syntax
(define (expand-initializer stx)
  (syntax-case* stx (unquote) symbolic-identifier=?
    [id                      (identifier? #'id)
                             #`(make-VariableInitializer #f #,(expand-identifier #'id) #f)]
    [(unquote id)            #`(make-VariableInitializer #f id #f)]
    [(id expr)               (identifier? #'id)
                             #`(make-VariableInitializer #f #,(expand-identifier #'id) #,(expand-expression #'expr))]
    [((unquote id) expr)     #`(make-VariableInitializer #f id #,(expand-expression #'expr))]
    [any                     (raise-syntax-error #f "bad syntax" (syntax->datum stx))]))

; syntax [boolean] -> syntax
(define (expand-statement stx [quote-expression? #t])
  (syntax-case* stx (function !begin !block if do while for for-in break continue return with switch !label throw try unquote unquote-splicing) symbolic-identifier=?
    [(!begin stmt ...)                  #`(wrap-begin (list #,@(map expand-javascript (syntax->list #'(stmt ...)))))]
    [(!block stmt ...)                  #`(make-BlockStatement
                                           #f
                                           (list #,@(map expand-javascript (syntax->list #'(stmt ...)))))]
    [(if expr pos)                      #`(make-IfStatement
                                           #f
                                           #,(expand-expression #'expr)
                                           #,(expand-statement #'pos)
                                           #f)]
    [(if expr pos neg)                  #`(make-IfStatement
                                           #f
                                           #,(expand-expression #'expr)
                                           #,(expand-statement #'pos)
                                           #,(expand-statement #'neg))]
    [(if arg ...)                       (raise-syntax-error #f "bad syntax" stx)]
    [(do stmt ...)                      #`(js:do       #,@(expand-do-arguments (syntax->list #'(stmt ...))))]
    [(while expr stmt ...)              #`(js:while    #,(expand-expression #'expr)
                                                       #,@(map expand-statement (syntax->list #'(stmt ...))))]
    [(for (init test incr) stmt ...)    #`(js:for      #,@(expand-for-init #'init)
                                                       #,@(expand-for-test #'test)
                                                       #,@(expand-for-incr #'incr)
                                                       #,@(map expand-statement (syntax->list #'(stmt ...))))]
    [(for (arg ...) stmt ...)           (raise-syntax-error #f "bad for arguments" stx)]
    [(for-in (item container) stmt ...) #`(js:for-in   #,(expand-for-in-item #'item) 
                                                       #,(expand-expression #'container)
                                                       #,@(map expand-statement (syntax->list #'(stmt ...))))]
    [(for-in (arg ...) stmt ...)        (raise-syntax-error #f "bad for-in arguments" stx)]
    [(break)                            #`(make-BreakStatement #f #f)]
    [(break label)                      #`(make-BreakStatement #f #,(expand-identifier #'label))]
    [(break arg ...)                    (raise-syntax-error #f "bad syntax" stx)]
    [(continue)                         #`(make-ContinueStatement #f #f)]
    [(continue label)                   #`(make-ContinueStatement #f #,(expand-identifier #'label))]
    [(continue arg ...)                 (raise-syntax-error #f "bad syntax" stx)]
    [(return)                           #`(make-ReturnStatement #f #f)]
    [(return expr)                      #`(make-ReturnStatement #f #,(expand-expression #'expr))]
    [(return arg ...)                   (raise-syntax-error #f "bad syntax" stx)]
    [(with expr stmt ...)               #`(js:with     #,(expand-expression #'expr)
                                                       #,@(map expand-statement (syntax->list #'(stmt ...))))]
    [(switch expr clause ...)           #`(js:switch   #,(expand-expression #'expr)
                                                       #,@(map expand-switch-clause (syntax->list #'(clause ...))))]
    [(!label id stmt)                   #`(make-LabelStatement
                                           #f
                                           #,(expand-identifier #'label)
                                           #,(expand-statement #'stmt))]
    [(!label arg ...)                   (raise-syntax-error #f "bad syntax" stx)]
    [(throw expr)                       #`(make-ThrowStatment #f #,(expand-expression #'expr))]
    [(throw arg ...)                    (raise-syntax-error #f "bad syntax" stx)]
    [(try stmt+clause ...)              #`(make-TryStatement #f #,@(expand-try-clauses #'(stmt+clause ...)))]
    [(unquote stmt)                     #`(quote-statement stmt)]
    [(unquote arg ...)                  (raise-syntax-error #f "bad syntax" stx)]
    [(unquote-splicing stmt-list)       #`(wrap-begin stmt-list)]
    [(unquote-splicing arg ...)         (raise-syntax-error #f "bad syntax" stx)]
    [expr                               (if quote-expression?
                                            #`(make-ExpressionStatement #f #,(expand-expression #'expr))
                                            (expand-expression #'expr))]))

; syntax -> (listof syntax)
(define (expand-do-arguments stx)
  (with-handlers ([exn:fail? (lambda (exn)
                               (error "while: bad arguments: ~a. Arguments were: ~s" 
                                      (exn-message exn) 
                                      (syntax->datum stx)))])
    (let loop ([stx stx] [accum null] [while-found? #f])
      (syntax-case stx ()
        [() 
         (if while-found?
             (reverse null)
             (error "expected one #:while argument, received none."))]
        [(last)
         (if (eq? (syntax->datum #'last) '#:while)
             (error "no #:while expression supplied.")
             (loop #'() (list* (expand-statement #'last) accum) while-found?))]
        [(first second rest ...)
         (if (eq? (syntax->datum #'first) '#:while)
             (if while-found?
                 (error "expected one #:while argument, received more than one.")
                 (loop #'(rest ...) (list* (expand-expression #'second) #'first accum) #t))
             (loop #'(second rest ...) (list* (expand-statement #'first) accum) while-found?))]))))

; syntax -> (listof syntax)
; TODO : Complete
(define (expand-for-init stx)
  (syntax-case* stx (_) symbolic-identifier=?
    [_    #`()]
    [decl #`(#:init #,(expand-declaration #'decl))]))

; syntax -> (listof syntax)
; TODO : Complete
(define (expand-for-test stx)
  (syntax-case* stx (_) symbolic-identifier=?
    [_    #`()]
    [expr #`(#:test #,(expand-expression #'expr))]))

; syntax -> (listof syntax)
; TODO : Complete
(define (expand-for-incr stx)
  (syntax-case* stx (_) symbolic-identifier=?
    [_    #`()]
    [expr #`(#:incr #,(expand-expression #'expr))]))

; syntax -> syntax
; TODO : Complete
(define (expand-for-in-item stx)
  (syntax-case* stx (var) symbolic-identifier=?
    [(var id1 id2) (and (identifier? #'id1) (identifier? #'id2))
                   (expand-declaration #'(var id1 id2))]
    [(var id)      (identifier? #'id)
                   (expand-declaration #'(var id))]
    [(var any ...) (identifier? #'id)
                   (raise-syntax-error #f "bad for-in declaration: initial values not allowed" stx)]
    [(id1 id2)     (and (identifier? #'id1) (identifier? #'id2))
                   (expand-expression #'(!all id1 id2))]
    [id            (identifier? #'id)
                   (expand-expression #'(!all id))]
    [any           (raise-syntax-error #f "bad for-in declaration" stx)]))

; syntax -> syntax
; TODO : Complete
(define (expand-switch-clause stx)
  (syntax-case* stx (case default) symbolic-identifier=?
    [(case expr stmt ...) #`(js:case #,(expand-expression #'expr) #,@(map expand-statement (syntax->list #'(stmt ...))))]
    [(default stmt ...)   #`(js:default #,@(map expand-statement (syntax->list #'(stmt ...))))]))

; syntax -> (listof syntax)
(define (expand-try-clauses stx)
  (define-values (body catch finally)
    (for/fold ([body null] [catch null] [finally null])
              ([stx (syntax->list stx)])
              (syntax-case* stx (catch finally) symbolic-identifier=?
                [(catch id stmt ...) 
                 (values body
                         (cons #`(make-CatchClause
                                  #f
                                  #,(expand-identifier #'id) 
                                  (make-BlockStatement #f (list #,@(map expand-statement (syntax->list #'(stmt ...))))))
                               catch)
                         finally)]
                [(finally stmt ...)
                 (values body
                         catch
                         (cons #`(make-BlockStatement #f (list #,@(map expand-statement (syntax->list #'(stmt ...))))) finally))]
                [stmt (values (cons (expand-statement #'stmt) body)
                              catch
                              finally)])))
  (case (length finally)
    [(0)  #`((make-BlockStatement #f (list #,@(reverse body))) (list #,@(reverse catch)) #f)]
    [(1)  #`((make-BlockStatement #f (list #,@(reverse body))) (list #,@(reverse catch)) #,(car finally))]
    [else (error (format "Expected zero or one finally clause(s), received ~s" (syntax->datum stx)))]))

; syntax -> syntax
(define (expand-expression stx)
  ; syntax -> symbol
  (define (js:op op-stx)
    (symbol-append 'js: (syntax->datum op-stx)))
  
  ; syntax-case/ops injects rules for prefix, infox, postfix and assignment operators:
  (syntax-case* stx (!array !object !index !dot !all ? new function unquote unquote-splicing) symbolic-identifier=?
    [(!array expr ...)             #`(js:array #,@(map expand-expression (syntax->list #'(expr ...))))]
    [(!object field ...)           #`(js:object #,@(map expand-field (syntax->list #'(field ...))))]
    [(!index container index)      #`(js:index #,(expand-expression #'container)
                                               #,(expand-expression #'index))]
    [(!index arg ...)              (raise-syntax-error #f "bad syntax" stx)]
    [(!dot expr id)                #`(make-DotReference #f #,(expand-expression #'expr) #,(expand-identifier #'id))]
    [(!dot expr ... id)            #`(make-DotReference #f #,(expand-expression #'(!dot expr ...)) #,(expand-identifier #'id))]
    [(!all expr ...)               #`(js:all #,@(map expand-expression (syntax->list #'(expr ...))))]
    [(? test pos neg)              #`(make-ConditionalExpression #f #,@(map expand-expression (syntax->list #'(test pos neg))))]
    [(? arg ...)                   (raise-syntax-error #f "bad syntax" stx)]
    [(new expr ...)                #`(js:new #,@(map expand-expression (syntax->list #'(expr ...))))]
    [(function (arg ...) stmt ...) #`(js:function (list #,@(map expand-identifier (syntax->list #'(arg ...)))) 
                                                  #,@(map expand-javascript (syntax->list #'(stmt ...))))]
    [(function arg ...)            (raise-syntax-error #f "bad anonymous function syntax" stx)]
    [(unquote expr)                #`(quote-expression expr)]
    [(unquote arg ...)             (raise-syntax-error #f "bad syntax" stx)]
    [(unquote-splicing stmt-list)  (raise-syntax-error #f "unquote-splicing is only allowed at a statement level" stx)]
    [(unquote-splicing arg ...)    (raise-syntax-error #f "bad syntax" stx)]
    ; Prefix and postfix operators:
    [(op expr)                     (or (scheme-prefix-operator? (syntax->datum #'op))
                                       (scheme-postfix-operator? (syntax->datum #'op)))
                                   #`(#,(js:op #'op) #,(expand-expression #'expr))]
    [(op expr ...)                 (or (scheme-prefix-operator? (syntax->datum #'op))
                                       (scheme-postfix-operator? (syntax->datum #'op)))
                                   (raise-syntax-error #f "one argument only" stx)]
    ; Infix operators:
    [(op expr ...)                 (infix-operator? (syntax->datum #'op))
                                   #`(#,(js:op #'op) #,@(map expand-expression (syntax->list #'(expr ...))))]
    ; Assignment operators:
    [(op expr1 expr2)              (assignment-operator? (syntax->datum #'op))
                                   #`(#,(js:op #'op) #,(expand-expression #'expr1) #,(expand-expression #'expr2))]
    
    [(op expr ...)                 (assignment-operator? (syntax->datum #'op))
                                   (raise-syntax-error #f "two arguments only" stx)]
    ; Function calls (first rule is a hack to get around an expression precedence bug in javascript.plt):
    [((function body ...) arg ...) #`(js:call (make-ParenExpression #f #,(expand-expression #'(function body ...))) #,@(map expand-expression (syntax->list #'(arg ...))))]
    [(fn arg ...)                  #`(js:call (parenthesize-anonymous-function #,(expand-expression #'fn))
                                              #,@(map expand-expression (syntax->list #'(arg ...))))]
    ; Literals and identifiers:
    [lit+id                        (or (identifier? #'lit+id)
                                       (quotable-literal? #'lit+id))
                                   (expand-literal+identifier #'lit+id)]
    [_                             (error "Bad Javascript syntax: ~s" (syntax->datum stx))]))

; syntax -> syntax
(define (expand-field stx)
  (syntax-case stx ()
    [(name expr) (identifier? #'name) 
                 #`(cons (make-Identifier #f 'name)     #,(expand-expression #'expr))]
    [(name expr) (string? (syntax->datum #'name))
                 #`(cons (make-StringLiteral #f name)  #,(expand-expression #'expr))]
    [(name expr) (integer? (syntax->datum #'name))
                 #`(cons (make-NumericLiteral #f name) #,(expand-expression #'expr))]))

; syntax -> syntax
(define (expand-literal+identifier stx)
  (syntax-case* stx (quote unquote) symbolic-identifier=?
    [(quote lit)   #'(quote-expression (quote lit))]
    [(unquote lit) #'lit]
    [id            (identifier? #'id)
                   #'(make-VarReference #f (make-Identifier #f 'id))]
    [lit           (quotable-literal? #'lit)
                   #'(quote-expression lit)]))

; syntax -> syntax
(define (expand-identifier stx)
  (syntax-case stx (unquote)
    [id           (identifier? #'id)
                  #'(js:id (quote id))]
    [(unquote id) #'id]))

; Helpers ----------------------------------------

;; (syntax -> boolean)
(define (quotable-literal? stx)
  (define datum (syntax->datum stx))
  (or (boolean? datum)
      (number? datum)
      (string? datum)
      (bytes? datum)))

; Provide statements -----------------------------

(provide expand-javascript
         expand-statement
         expand-expression)
