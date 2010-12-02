#lang scheme/base

(require "../base.ss")

(require scheme/pretty
         (javascript-in config)
         (unlib-in log syntax symbol)
         "expander.ss"
         "op.ss"
         "struct.ss"
         (for-template scheme/base
                       scheme/match
                       (prefix-in js: "lang.ss")
                       "quote.ss"
                       "struct.ss"))

; syntax [boolean] -> syntax
(define (expand-javascript stx [quote-expression? #t])
  (syntax-case* stx (var function) symbolic-identifier=?
    [(var init ...)                   (expand-declaration #'(var init ...))]
    [(function id (arg ...) stmt ...) (expand-declaration #'(function id (arg ...) stmt ...))]
    [stmt                             (expand-statement #'stmt quote-expression?)]))

; syntax -> syntax
(define (expand-declaration stx)
  (syntax-case* stx (var function) symbolic-identifier=?
    [(expander arg ...)               (javascript-expander-syntax? #'expander)
                                      (let ([expanded-stx (javascript-expand #'(expander arg ...))])
                                        (syntax-case* expanded-stx (js) symbolic-identifier=?
                                          [(js expr)     (expand-declaration #'expr)]
                                          [(js expr ...) (expand-declaration #'(!begin expr ...))]))]
    [(var init ...)                   #`(make-VariableDeclaration #f (list #,@(map expand-initializer (syntax->list #'(init ...)))))]
    [(function id (arg ...) stmt ...) (javascript-identifier-guard #'id)
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
    [(function any ...)               (raise-syntax-error #f "bad JS syntax" (syntax->datum stx))]))

; syntax -> syntax
(define (expand-initializer stx)
  (syntax-case* stx (unquote) symbolic-identifier=?
    [id                      (javascript-identifier-guard #'id)
                             #`(make-VariableInitializer #f #,(expand-identifier #'id) #f)]
    [(unquote id)            #`(make-VariableInitializer #f id #f)]
    [(id expr)               (javascript-identifier-guard #'id)
                             #`(make-VariableInitializer #f #,(expand-identifier #'id) #,(expand-expression #'expr))]
    [((unquote id) expr)     #`(make-VariableInitializer #f id #,(expand-expression #'expr))]
    [any                     (raise-syntax-error #f "bad JS syntax" (syntax->datum stx))]))

; syntax [boolean] -> syntax
(define (expand-statement stx [quote-expression? #t])
  (syntax-case* stx (function !begin !block !raw if do while for for-in break continue return with switch !label throw try unquote unquote-splicing) symbolic-identifier=?
    [(expander arg ...)                 (javascript-expander-syntax? #'expander)
                                        (let ([expanded-stx (javascript-expand #'(expander arg ...))])
                                          (syntax-case* expanded-stx (js) symbolic-identifier=?
                                            [(js expr)     (expand-statement #'expr)]
                                            [(js expr ...) (expand-statement #'(!begin expr ...))]))]
    [(!begin stmt ...)                  #`(wrap-begin (list #,@(map expand-javascript (syntax->list #'(stmt ...)))))]
    [(!block stmt ...)                  #`(make-BlockStatement
                                           #f
                                           (list #,@(map expand-javascript (syntax->list #'(stmt ...)))))]
    [(!raw expr)                       #`(make-ExpressionStatement
                                          #f
                                          (make-RawExpression
                                           #f
                                           #,(syntax-case* #'expr (quote unquote) symbolic-identifier=?
                                               [(unquote val) #'val]
                                               [(quote val)   #'(quote val)]
                                               [val           (quotable-literal? #'val)
                                                              #'val])))]
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
    [(if arg ...)                       (raise-syntax-error #f "bad JS syntax" stx)]
    [(do stmt ...)                      #`(js:do       #,@(expand-do-arguments (syntax->list #'(stmt ...))))]
    [(while expr stmt ...)              #`(js:while    #,(expand-expression #'expr)
                                                       #,@(map expand-statement (syntax->list #'(stmt ...))))]
    [(for (init test incr) stmt ...)    #`(js:for      #,@(expand-for-init #'init)
                                                       #,@(expand-for-test #'test)
                                                       #,@(expand-for-incr #'incr)
                                                       #,@(map expand-statement (syntax->list #'(stmt ...))))]
    [(for (arg ...) stmt ...)           (raise-syntax-error #f "bad JS for arguments" stx)]
    [(for-in (item container) stmt ...) #`(js:for-in   #,(expand-for-in-item #'item) 
                                                       #,(expand-expression #'container)
                                                       #,@(map expand-statement (syntax->list #'(stmt ...))))]
    [(for-in (arg ...) stmt ...)        (raise-syntax-error #f "bad JS for-in arguments" stx)]
    [(break)                            #`(make-BreakStatement #f #f)]
    [(break label)                      #`(make-BreakStatement #f #,(expand-identifier #'label))]
    [(break arg ...)                    (raise-syntax-error #f "bad JS syntax" stx)]
    [(continue)                         #`(make-ContinueStatement #f #f)]
    [(continue label)                   #`(make-ContinueStatement #f #,(expand-identifier #'label))]
    [(continue arg ...)                 (raise-syntax-error #f "bad JS syntax" stx)]
    [(return)                           #`(make-ReturnStatement #f #f)]
    [(return expr)                      #`(make-ReturnStatement #f #,(expand-expression #'expr))]
    [(return arg ...)                   (raise-syntax-error #f "bad JS syntax" stx)]
    [(with expr stmt ...)               #`(js:with     #,(expand-expression #'expr)
                                                       #,@(map expand-statement (syntax->list #'(stmt ...))))]
    [(switch expr clause ...)           #`(js:switch   #,(expand-expression #'expr)
                                                       #,@(map expand-switch-clause (syntax->list #'(clause ...))))]
    [(!label id stmt)                   #`(make-LabelStatement
                                           #f
                                           #,(expand-identifier #'label)
                                           #,(expand-statement #'stmt))]
    [(!label arg ...)                   (raise-syntax-error #f "bad JS syntax" stx)]
    [(throw expr)                       #`(make-ThrowStatement #f #,(expand-expression #'expr))]
    [(throw arg ...)                    (raise-syntax-error #f "bad JS syntax" stx)]
    [(try stmt+clause ...)              #`(make-TryStatement #f #,@(expand-try-clauses #'(stmt+clause ...)))]
    [(unquote stmt)                     #`(quote-statement stmt)]
    [(unquote arg ...)                  (raise-syntax-error #f "bad JS syntax" stx)]
    [(unquote-splicing stmt-list)       #`(wrap-begin stmt-list)]
    [(unquote-splicing arg ...)         (raise-syntax-error #f "bad JS syntax" stx)]
    [expr                               (if quote-expression?
                                            #`(make-ExpressionStatement #f #,(expand-expression #'expr))
                                            (expand-expression #'expr))]))

; syntax -> (listof syntax)
(define (expand-do-arguments stx)
  (with-handlers ([exn:fail? (lambda (exn)
                               (raise-syntax-error #f (format "bad JS syntax: ~a" (exn-message exn)) stx))])
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
    [(var id1 id2) (and (javascript-identifier-guard #'id1) (javascript-identifier-guard #'id2))
                   (expand-declaration #'(var id1 id2))]
    [(var id)      (javascript-identifier-guard #'id)
                   (expand-declaration #'(var id))]
    [(var any ...) (raise-syntax-error #f "bad JS for-in declaration: initial values not allowed" stx)]
    [(id1 id2)     (and (javascript-identifier-guard #'id1) (javascript-identifier-guard #'id2))
                   (expand-expression #'(!all id1 id2))]
    [id            (javascript-identifier-guard #'id)
                   (expand-expression #'(!all id))]
    [any           (raise-syntax-error #f "bad JS for-in declaration" stx)]))

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
    [else (raise-syntax-error #f "bad JS syntax: too many finally clause(s)" stx)]))

; syntax -> syntax
(define (expand-expression stx)
  ; syntax -> symbol
  (define (js:op op-stx)
    (symbol-append 'js: (syntax->datum op-stx)))
  
  ; syntax-case/ops injects rules for prefix, infox, postfix and assignment operators:
  (syntax-case* stx (!array !object !regexp !index !dot !all ? !raw new function quote unquote unquote-splicing) symbolic-identifier=?
    [(expander arg ...)                 (javascript-expander-syntax? #'expander)
                                        (let ([expanded-stx (javascript-expand #'(expander arg ...))])
                                          (syntax-case* expanded-stx (js) symbolic-identifier=?
                                            [(js expr)     (expand-expression #'expr)]
                                            [(js expr ...) (expand-expression #'(!begin expr ...))]))]
    [(!array ,@exprs)                   #`(make-ArrayLiteral #f (map quote-expression exprs))]
    [(!array expr ...)                  #`(make-ArrayLiteral #f (list #,@(map expand-expression (syntax->list #'(expr ...)))))]
    [(!object ,@exprs)                  #`(make-ObjectLiteral #f (map (match-lambda
                                                                        [(list-rest key val)
                                                                         (js:field key val)]
                                                                        [other (error "bad javascript field" other)])
                                                                      exprs))]
    [(!object field ...)                #`(make-ObjectLiteral #f (list #,@(map expand-field (syntax->list #'(field ...)))))]
    [(!regexp arg ...)                  (expand-regexp-expr #'(!regexp arg ...))]
    [(!index container index)           #`(make-BracketReference
                                           #f
                                           #,(expand-expression #'container)
                                           #,(expand-expression #'index))]
    [(!index arg ...)                   (raise-syntax-error #f "bad JS syntax" stx)]
    [(!dot expr (!index id index))      #`(make-BracketReference
                                           #f
                                           (make-DotReference
                                            #f
                                            #,(expand-expression #'expr)
                                            #,(expand-identifier #'id)) 
                                           #,(expand-expression #'index))]
    [(!dot expr (id arg ...))           #`(make-CallExpression
                                           #f
                                           (make-DotReference
                                            #f 
                                            #,(expand-expression #'expr)
                                            #,(expand-identifier #'id)) 
                                           (list #,@(map expand-expression (syntax->list #'(arg ...)))))]
    [(!dot expr id)                     #`(make-DotReference
                                           #f 
                                           #,(expand-expression #'expr)
                                           #,(expand-identifier #'id))]
    [(!dot expr ... (!index id index))  #`(make-BracketReference
                                           #f
                                           (make-DotReference
                                            #f
                                            #,(expand-expression #'(!dot expr ...))
                                            #,(expand-identifier #'id)) 
                                           #,(expand-expression #'index))]
    [(!dot expr ... (id arg ...))       #`(make-CallExpression
                                           #f
                                           (make-DotReference
                                            #f
                                            #,(expand-expression #'(!dot expr ...))
                                            #,(expand-identifier #'id))
                                           (list #,@(map expand-expression (syntax->list #'(arg ...)))))]
    [(!dot expr ... id)                #`(make-DotReference
                                          #f
                                          #,(expand-expression #'(!dot expr ...))
                                          #,(expand-identifier #'id))]
    [(!all expr ...)                    #`(js:all #,@(map expand-expression (syntax->list #'(expr ...))))]
    [(? test pos neg)                   #`(make-ConditionalExpression #f #,@(map expand-expression (syntax->list #'(test pos neg))))]
    [(? arg ...)                        (raise-syntax-error #f "bad JS syntax" stx)]
    [(!raw expr)                        #`(make-ParenExpression
                                           #f
                                           (make-RawExpression
                                            #f
                                            #,(syntax-case* #'expr (quote unquote) symbolic-identifier=?
                                                [(unquote val) #'val]
                                                [(quote val)   #'(quote val)]
                                                [val           (quotable-literal? #'val)
                                                               #'val])))]
    [(new class expr ...)               #`(make-NewExpression
                                           #f
                                           #,(expand-expression #'class)
                                           (list #,@(map expand-expression (syntax->list #'(expr ...)))))]
    [(function (arg ...) stmt ...)      #`(make-FunctionExpression
                                           #f
                                           #f
                                           (list #,@(map expand-identifier (syntax->list #'(arg ...)))) 
                                           (list #,@(map expand-javascript (syntax->list #'(stmt ...)))))]
    [(function arg ...)                 (raise-syntax-error #f "bad JS syntax" stx)]
    [(unquote expr)                     #`(quote-expression expr)]
    [(unquote arg ...)                  (raise-syntax-error #f "bad JS syntax" stx)]
    [(unquote-splicing stmt-list)       (raise-syntax-error #f "bad JS syntax: unquote-splicing is only allowed at a statement level" stx)]
    [(unquote-splicing arg ...)         (raise-syntax-error #f "bad JS syntax" stx)]
    ; Prefix and postfix operators:
    [(op expr)                          (or (scheme-prefix-operator? (syntax->datum #'op))
                                            (scheme-postfix-operator? (syntax->datum #'op)))
                                        #`(#,(js:op #'op) #,(expand-expression #'expr))]
    [(op expr ...)                      (or (scheme-prefix-operator? (syntax->datum #'op))
                                            (scheme-postfix-operator? (syntax->datum #'op)))
                                        (raise-syntax-error #f "bad JS syntax: one argument only" stx)]
    ; Infix operators:
    [(op expr ...)                      (infix-operator? (syntax->datum #'op))
                                        #`(#,(js:op #'op) #,@(map expand-expression (syntax->list #'(expr ...))))]
    ; Assignment operators:
    [(op expr1 expr2)                   (assignment-operator? (syntax->datum #'op))
                                        #`(#,(js:op #'op) #,(expand-expression #'expr1) #,(expand-expression #'expr2))]
    
    [(op expr ...)                      (assignment-operator? (syntax->datum #'op))
                                        (raise-syntax-error #f "bad JS syntax: two arguments only" stx)]
    ; Function calls (first rule is a hack to get around an expression precedence bug in javascript.plt):
    [((function body ...) arg ...)      #`(js:call (make-ParenExpression #f #,(expand-expression #'(function body ...))) #,@(map expand-expression (syntax->list #'(arg ...))))]
    [(quote arg)                        (expand-literal+identifier #'(quote arg))]
    [(quote arg ...)                    (raise-syntax-error #f "bad JS syntax: one argument only" stx)]
    [(fn arg ...)                       #`(make-CallExpression #f
                                                               (parenthesize-anonymous-function #,(expand-expression #'fn))
                                                               (list #,@(map expand-expression (syntax->list #'(arg ...)))))]
    ; Literals and identifiers:
    [lit+id                             (or (javascript-identifier-guard #'lit+id)
                                            (quotable-literal? #'lit+id))
                                        (expand-literal+identifier #'lit+id)]
    [_                                  (raise-syntax-error #f "bad JS syntax" stx)]))

; syntax -> syntax
(define (expand-field stx)
  (syntax-case stx ()
    [(name expr) (javascript-identifier-guard #'name) 
                 #`(cons (make-Identifier #f 'name)    #,(expand-expression #'expr))]
    [(name expr) (string? (syntax->datum #'name))
                 #`(cons (make-StringLiteral #f name)  #,(expand-expression #'expr))]
    [(name expr) (integer? (syntax->datum #'name))
                 #`(cons (make-NumericLiteral #f name) #,(expand-expression #'expr))]))

; syntax -> syntax
(define (expand-literal+identifier stx)
  (syntax-case* stx (quote unquote) symbolic-identifier=?
    [(quote lit)   #'(quote-expression (quote lit))]
    [(unquote lit) #'lit]
    [id            (javascript-identifier-guard #'id)
                   #`(make-VarReference #f #,(expand-identifier #'id))]
    [lit           (quotable-literal? #'lit)
                   #'(quote-expression lit)]))

; syntax -> syntax
(define (expand-identifier stx)
  (syntax-case stx (unquote)
    [id           (javascript-identifier-guard #'id)
                  (if (or (javascript-identifier? #'id)
                          (memq (syntax->datum #'id) '(this null)))
                      #'(js:id (quote id))
                      (raise-syntax-error #f "bad JS syntax: invalid JS identifier" stx))]
    [(unquote id) #'id]))

; Helpers ----------------------------------------

; syntax -> boolean
(define (quotable-literal? stx)
  (define datum (syntax->datum stx))
  (or (boolean? datum)
      (number? datum)
      (string? datum)
      (bytes? datum)))

; syntax -> boolean
(define javascript-identifier?
  (let ([identifier-regexp  #rx"^[a-zA-Z_$][a-zA-Z0-9_$]*$"])
    (lambda (stx)
      (define datum (syntax->datum stx))
      (and (symbol? datum)
           (regexp-match identifier-regexp (symbol->string datum))
           (not (memq datum (lexical-keywords)))))))

; syntax -> syntax
(define (expand-regexp-expr stx)
  
  ; (U syntax #f)
  (define value-stx
    #f)
  
  ; (U syntax #f)
  (define global-stx
    #f)
  
  ; (U syntax #f)
  (define ci-stx
    #f)
  
  ; syntax -> syntax
  (define (parse-value-expr value-stx)
    (syntax-case value-stx (unquote)
      [expr 
       ; Guard:
       (string? (syntax->datum #'expr))
       ; Value:
       value-stx]
      [(unquote expr) #'expr]
      [_ (raise-syntax-error #f "bad JS syntax: regexp argument: expected string literal or unquoted string expression" stx value-stx)]))
  
  ; syntax -> syntax
  (define (parse-keyword-expr val-stx)
    (syntax-case val-stx (unquote)
      [#t val-stx]
      [#f val-stx]
      [(unquote expr) #'expr]
      [_ (raise-syntax-error #f "bad JS: regexp argument: expected boolean literal or unquoted boolean expression" stx val-stx)]))
  
  ; -> syntax
  (define (parse-end)
    #`(js:regexp #,value-stx 
                 #,@(if global-stx
                        (list (datum->syntax #f '#:global?) global-stx)
                        null)
                 #,@(if ci-stx
                        (list (datum->syntax #f '#:ci?) ci-stx)
                        null)))
  
  ; syntax -> syntax
  (define (parse-keywords keys-stx)
    (syntax-case keys-stx ()
      [() (parse-end)]
      [(#:global? expr rest ...)
       (begin (if global-stx
                  (raise-syntax-error #f "bad JS syntax: too many #:global? keywords" stx #'expr)
                  (set! global-stx (parse-keyword-expr #'expr)))
              (parse-keywords #'(rest ...)))]
      [(#:ci? expr rest ...)
       (begin (if ci-stx
                  (raise-syntax-error #f "bad JS syntax: too many #:ci? keywords" stx #'expr)
                  (set! ci-stx (parse-keyword-expr #'expr)))
              (parse-keywords #'(rest ...)))]
      [(kw expr rest ...)
       (raise-syntax-error #f "bad regexp keyword" stx #'kw)]
      [other 
       (raise-syntax-error #f "bad regexp keywords" stx #'other)]))
  
  (syntax-case stx ()
    [(_ value key ...)
     (begin (set! value-stx (parse-value-expr #'value))
            (parse-keywords #'(key ...)))]))

; syntax -> boolean
(define (javascript-identifier-guard stx)
  (cond [(memq (syntax->datum stx)
               (if (eq? (quote-case-restriction) 'lower)
                   lowercase-quote-symbols
                   uppercase-quote-symbols))
         (raise-syntax-error 'mirrors/javascript 
                             (if (eq? (quote-case-restriction) 'lower)
                                 "cannot use this identifier here (possible double quoting error): switch the surrounding quote macro to \"JS\" or another uppercase equivalent"
                                 "cannot use this identifier here (possible double quoting error): switch the surrounding quote macro to \"js\" or another lowercase equivalent")
                             #'name)]
        [else (identifier? stx)]))

; Provide statements -----------------------------

(provide expand-javascript
         expand-statement
         expand-expression)
