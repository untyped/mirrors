#lang scribble/doc

@(require (file "base.ss"))

@(define-eval js-eval (for-syntax scheme/base) (planet untyped/mirrors/javascript/javascript))

@title[#:tag "javascript-syntax"]{Constructing Javascript fragments}

@defmodule[(planet untyped/mirrors/javascript/syntax)]

@section{Javascript macros}

Mirrors provides the following macros for creating blocks of Javascript:

@defform[(js js-stmt ...)]{
Builds a block of Javascript. See @secref["js-stmt"] for the syntax of @scheme[js-stmt].

@examples[
  #:eval js-eval
  (let ([message "Hello world"])
    (pretty-print (js (alert ,message))))]

To prevent double-quoting errors, it is a syntax error to use the following Mirrors quote forms within a @scheme[js] block: @scheme[xml], @scheme[xml*], @scheme[xml-attrs], @scheme[xml-attrs*], @scheme[opt-xml], @scheme[opt-xml-attr], @scheme[js], @scheme[opt-js]:

@interaction[
  #:eval js-eval
  (js (js))]

You can get around this restriction using the uppercase form, @scheme[JS], described below.}

@defform[(JS js-stmt ...)]{
Like @scheme[js], but permits the use of the lowercase Mirrors quote forms as @scheme[js-stmt]@schemeidfont{s}:

@interaction[
  #:eval js-eval
  (code:line (pretty-print (JS (js))) (code:comment "'js()' function call"))]

It is a syntax error to use the following identifiers in expression position within a @scheme[JS] block: @scheme[XML], @scheme[XML*], @scheme[XML-ATTRS], @scheme[XML-ATTRS*], @scheme[OPT-XML], @scheme[OPT-XML-ATTR], @scheme[JS], @scheme[OPT-JS]:

@interaction[
  #:eval js-eval
  (JS (JS))]}

@defform[(opt-js boolean-expr js-stmt ...)]{

Syntactic shorthand for:

@schemeblock[
  (if boolean-expr
      (js js-stmt ...)
      (js))]

@examples[
  #:eval js-eval
  (javascript->string (opt-js #t (alert "This statement will be printed....")))
  (javascript->string (opt-js #f (alert "...but this statement won't.")))]}

@defform[(OPT-JS boolean-expr js-stmt ...)]{
Like @scheme[opt-js], but follows the same double-quoting rules as @scheme[JS].}

@section[#:tag "js-stmt"]{Javascript syntax}

The forms above use the same parenthetical Javascript syntax:

@schemegrammar*[
  #:literals (function !begin !block if do while for for-in break continue return with switch !label throw try unquote unquote-splicing 
               !array !object !index !dot !all ? new function null this id _)
  [js-stmt          js-decl
                     (custom-syntax-id js-stmt ...)
                     (!begin js-stmt ...)
                     (!block js-stmt ...)
                     (!raw string-expr)
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
                     (!raw string-expr)
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
   [js-operator      (code:line ... (code:comment "Javascript operator: =, +, etc"))]
   [custom-syntax-id (code:line id (code:comment "bound by define-js-syntax"))]
   [regexp-key       (code:line #:global? boolean-literal)
                     (code:line #:global? (unquote boolean-expr))
                     (code:line #:ci? boolean-literal)
                     (code:line #:ci? (unquote boolean-expr))]
   [dot-expr         (code:line id    (code:comment "property-style: a.b"))
                     (code:line (!index id expr) (code:comment "array-style: a.b[1]"))
                     (code:line (id js-expr ...) (code:comment "method-style: a.b(1, 2)"))]
   [opt-decl         _
                     var-decl]
   [opt-expr         _
                     expr]]
                     
@scheme[!raw] forms are treated as statements or expressions depending on their context. Semicolons and parentheses are added automatically depending on the interpretation of the block:

@examples[
  #:eval js-eval
  (code:comment "raw statements are followed by semicolons:")
  (display (javascript->pretty-string
            (js (alert "First statement")
            (!raw "alert(\"Second sta tement\")")
            (alert "Third statement"))))
  (code:comment "raw expressions are surrounded by parentheses:")
  (display (javascript->pretty-string
            (js (alert (+ 1 (!raw "2") 3)))))]

@section{Custom Javascript syntax}

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

@defproc[(javascript? [val any]) boolean?]{
Returns @scheme[#t] if @scheme[val] is a Javascript declaration, statement or expression.}

@defproc[(javascript-declaration? [val any]) boolean?]{
Returns @scheme[#t] if @scheme[val] is a Javascript declaration.}

@defproc[(javascript-statement? [val any]) boolean?]{
Returns @scheme[#t] if @scheme[val] is a Javascript statement.}

@defproc[(javascript-expression? [val any]) boolean?]{
Returns @scheme[#t] if @scheme[val] is a Javascript expression.}
