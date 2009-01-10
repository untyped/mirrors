#lang scribble/doc

@(require (file "base.ss"))

@(define-eval js-eval (planet untyped/mirrors/javascript/javascript))

@title[#:tag "javascript-syntax"]{Javascript syntax}

@defmodule[(planet untyped/mirrors/javascript/syntax)]

Mirrors provides a set of macros for rendering blocks of Javascript, intended to replace Jay McCarthy's @tt{javascript.plt} (which is no longer in development). Almost no checking is performed to make sure the Javascript code makes sense.

@defform/subs[
  #:literals (function !begin !block if do while for for-in break continue return with switch !label throw try unquote unquote-splicing 
               !array !object !index !dot !all ? new function null this id _)
  (js js-stmt ...)
  ([js-stmt    js-decl
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
   [js-decl    (function id (id ...) js-stmt ...)
               var-decl]
   [var-decl   (var js-init ...)]
   [js-init    id
               [id js-expr]
               (unquote expr)
               [id (unquote expr)]]
   [js-expr    (!array js-expr ...)
               (!object [property js-expr] ...)
               (!index js-expr js-expr)
               (!dot js-expr ...)
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
   [regexp-key (code:line #:global? boolean-literal)
               (code:line #:global? (unquote boolean-expr))
               (code:line #:ci? boolean-literal)
               (code:line #:ci? (unquote boolean-expr))]
   [opt-decl   _
               var-decl]
   [opt-expr   _
               expr])]{
Builds a Javascript declaration, statement or expression. Unquote can be used to insert Scheme expressions resulting in Boolean, numeric, string, symbol, bytes or URL values.}

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
