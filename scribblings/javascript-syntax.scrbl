#lang scribble/doc

@(require (file "base.ss"))

@(define-eval js-eval (for-syntax scheme/base) (planet untyped/mirrors/javascript/javascript))

@title[#:tag "javascript-syntax"]{Javascript syntax}

@defmodule[(planet untyped/mirrors/javascript/syntax)]

Mirrors provides a set of macros for rendering blocks of Javascript, intended to replace Jay McCarthy's @tt{javascript.plt} (which is no longer in development). Very little checking is performed to make sure the Javascript code makes sense.

@defform/subs[
  #:literals (function !begin !block if do while for for-in break continue return with switch !label throw try unquote unquote-splicing 
               !array !object !index !dot !all ? new function null this id _)
  (js js-stmt ...)
  ([js-stmt          js-decl
                     (custom-syntax-id js-stmt ...)
                     (!begin js-stmt ...)
                     (!block js-stmt ...)
                     (!regexp str regexp-key ...)
                     (if js-expr js-stmt js-stmt)
                     (do js-stmt ... js-expr)
                     (while js-expr js-stmt ...)
                     (for (opt-decl opt-expr opt-expr) js-stmt ...)
                     (for-in (decl js-expr) js-stmt ...)
                     (break)
                     (break id)
                     (continue)
                     (continue id)
                     (return)
                     (return js-expr)
                     (with js-expr js-stmt ...)
                     (switch switch-clause ...)
                     (!label id js-stmt)
                     (throw js-expr)
                     (try try-clause ...)
                     (unquote expr)
                     (unquote-splicing expr)
                     js-expr]
   [js-decl          (custom-syntax-id js-decl ...)
                     (function id (id ...) js-stmt ...)
                     (var js-init ...)]
   [js-init          id
                     [id js-expr]
                     (unquote expr)
                     [id (unquote expr)]]
   [js-expr          (custom-syntax-id js-expr ...)
                     (js-operator js-expr ...)
                     (!array js-expr ...)
                     (!object [property js-expr] ...)
                     (!index js-expr js-expr)
                     (!dot js-expr dot-expr ...)
                     (!all js-expr ...)
                     (? js-expr js-expr js-expr)
                     (new js-expr js-expr ...)
                     (function (id ...) js-stmt ...)
                     (unquote expr)
                     null
                     this
                     id
                     boolean-literal
                     number-literal
                     string-literal
                     (quote symbol-literal)]
   [js-operator      (code:line ...) (code:comment "symbolic Javascript operator: =, +, etc")]
   [custom-syntax-id id]
   [regexp-key       (code:line #:global? boolean-literal)
                     (code:line #:global? (unquote boolean-expr))
                     (code:line #:ci? boolean-literal)
                     (code:line #:ci? (unquote boolean-expr))]
   [dot-expr         (code:line id    (code:comment "property-style accessor : a.b"))
                     (code:line (!index id expr) (code:comment "array-index-style accessor: a.b[1]"))
                     (code:line (id js-expr ...) (code:comment "method-style accessor: a.b(1, 2)"))]
   [opt-decl         _
                     var-decl]
   [opt-expr         _
                     expr])]{
Builds a Javascript declaration, statement or expression. Unquote can be used to insert Scheme expressions resulting in Boolean, numeric, string, symbol, bytes or URL values.

@scheme[custom-syntax-id] indicates an identifier bound with @scheme[define-javascript-syntax].}

@defform*[[(define-javascript-syntax (id arg ...) js-expr)
           (define-javascript-syntax id js-transformer)
           (define-javascript-syntax id js-transformer expr-transformer)]]{
Defines a custom syntactic form for use in @scheme[js] blocks.

The first form above behaves like @scheme[define-syntax-rule]. @scheme[js-expr] should be a regular @scheme[js] block.

@examples[
  #:eval js-eval
  (define-javascript-syntax (!max a b)
    (js (? (> a b) a b)))
  (javascript->string (js (!max 1 2)))
  (!max 1 2)]
  
The second and third forms behave like @scheme[define-match-expander]. @scheme[js-transformer] is a syntax transformer procedure used during Javascript expansion, which accepts a Javascript form as an input and returns a complete @scheme[js] block representing the expansion. @scheme[expr-transformer] is a transformer procedure that is used in regular Scheme expansion. When @scheme[js-transformer] is omitted, use of the syntax form outside of a Javascript block results in a syntax error.

@examples[
  #:eval js-eval
  (define-javascript-syntax !min
    (lambda (stx)
      (syntax-case stx ()
        [(_ a b) #'(js (? (< a b) a b))]))
    (lambda (stx)
      (syntax-case stx ()
        [(_ a b) #'(if (< a b) a b)])))
  (javascript->string (js (!min 1 2)))
  (!min 1 2)]}

@defform[(opt-js boolean-expr js-stmt ...)]{

Syntactic shorthand for:

@schemeblock[
  (if boolean-expr
      (js js-stmt ...)
      (js))]

Only works at the statement level.

@examples[
  #:eval js-eval
  (javascript->string
   (opt-js #t (alert "This alert will be shown....")))
  (javascript->string
   (opt-js #f (alert "...but this alert won't.")))]}

@defproc[(javascript? [val any]) boolean?]{
Returns @scheme[#t] if @scheme[val] is a Javascript declaration, statement or expression.}

@defproc[(javascript-declaration? [val any]) boolean?]{
Returns @scheme[#t] if @scheme[val] is a Javascript declaration.}

@defproc[(javascript-statement? [val any]) boolean?]{
Returns @scheme[#t] if @scheme[val] is a Javascript statement.}

@defproc[(javascript-expression? [val any]) boolean?]{
Returns @scheme[#t] if @scheme[val] is a Javascript expression.}
