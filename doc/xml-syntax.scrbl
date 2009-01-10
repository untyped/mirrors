#lang scribble/doc

@(require (file "base.ss"))

@(define-eval xml-eval (planet untyped/mirrors/xml/xml))

@title[#:tag "xml-syntax"]{XML syntax}

@defmodule[(planet untyped/mirrors/xml/syntax)]

Mirrors proides two macros for building XML. Both of these forms have the same syntax, which is inspired by the list syntax used in Neil van Dyke's @italic{HtmlPrag} package:

@defform/subs[
  #:literals (\@ & U !comment !cdata !pi !raw id xml attribute quotable-value symbol integer)
  (xml xml-expr ...)
  ([xml-expr   quotable-value
               (tag xml-expr ...)
               (tag (\@ attr-expr ...) xml-expr ...)
               (& code-expr)
               (!comment value-expr)
               (!cdata value-expr)
               (!pi value-expr)
               (!raw value-expr)
               (unquote xml)
               (unquote-splicing (listof xml))]
   [tag        id]
   [code-expr  id
               integer
               (unquote (U symbol integer))]
   [value-expr quotable-value
               (unquote quotable-value)]
   [attr-expr  (id quotable-value)
               (id (unquote quotable-value))
               (unquote attribute)
               (unquote-splicing (listof attribute))])]{
Builds a block of XML, optimising any chunks of static markup into @scheme[raw] blocks. For example:

@interaction[
  #:eval xml-eval
  (let ([content "content"])
    (pretty-print (xml (span (\@ [title "title"]) ,content))))]}
   
@defform[(xml* xml-expr ...)]{
The same as @scheme[xml] except that no markup optimisation is performed:

@interaction[
  #:eval xml-eval
  (let ([content "content"])
    (pretty-print (xml* (span (\@ [title "title"]) ,content))))]

It takes slightly longer to render the XML trees produced with @scheme[xml*] as strings, but the full structure of the tree is preserved.}

@defform[(xml-attrs attr-expr ...)]{
Builds a list of @scheme[attribute] structures. Useful in conjunction with the @scheme[unquote-splicing] form of @scheme[attr-expr]:

@interaction[
  #:eval xml-eval
  (let ([attrs (xml-attrs [title "title"] [href "href"])])
    (xml->string (xml (a (\@ ,@attrs) "text"))))]}
