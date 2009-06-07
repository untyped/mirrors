#lang scribble/doc

@(require (planet untyped/mirrors/base)
          (file "base.ss"))

@(define-eval xml-eval (for-syntax scheme/base) (planet untyped/mirrors/xml/xml))

@title[#:tag "xml-syntax"]{XML syntax}

@defmodule[(planet untyped/mirrors/xml/syntax)]

@section{XML macros}

Mirrors provides the following macros for building XML:

@defform[(xml xml-expr ...)]{
Builds a block of XML, optimising any immutable blocks of markup into @scheme[!raw] blocks to reduce memory consumption and rendering time. See @secref["xml-expr"] for the syntax of @scheme[xml-expr].

@examples[
  #:eval xml-eval
  (let ([content "content"])
    (pretty-print (xml (span (\@ [title "title"]) ,content))))]

To prevent double-quoting errors, it is a syntax error to use the following Mirrors quote forms within an @scheme[xml] block: @scheme[xml], @scheme[xml*], @scheme[xml-attrs], @scheme[xml-attrs*], @scheme[opt-xml], @scheme[opt-xml-attr], @scheme[js], @scheme[opt-js]:

@interaction[
  #:eval xml-eval
  (xml (xml "this is bad"))]

You can get around this restriction using the uppercase form, @scheme[XML], described below.}

@defform[(XML xml-expr ...)]{
Like @scheme[xml], but permits the use of the lowercase Mirrors quote forms within the @scheme[xml-expr]@schemeidfont{s}:

@interaction[
  #:eval xml-eval
  (XML (xml "this is okay"))]

It is a syntax error to use the following identifiers in expression position within an @scheme[XML] block: @scheme[XML], @scheme[XML*], @scheme[XML-ATTRS], @scheme[XML-ATTRS*], @scheme[OPT-XML], @scheme[OPT-XML-ATTR], @scheme[JS], @scheme[OPT-JS]:

@interaction[
  #:eval xml-eval
  (XML (XML "this is bad"))]}

@defform[(xml* xml-expr ...)]{
Like @scheme[xml], but skips the optimisation of static markup, leaving the element structure intact for later traversal and manipulation:

@examples[
  #:eval xml-eval
  (let ([content "content"])
    (pretty-print (xml* (span (\@ [title "title"]) ,content))))]}

@defform[(XML* xml-expr ...)]{
Like @scheme[xml*], but follows the same double-quoting rules as @scheme[XML].}

@defform[(xml-attrs attr-expr ...)]{
Builds a list of @scheme[attribute] structures. Useful in conjunction with the @scheme[unquote-splicing] form of @scheme[attr-expr]:

@interaction[
  #:eval xml-eval
  (let ([attrs (xml-attrs [title "title"] [href "href"])])
    (xml->string (xml (a (\@ ,@attrs) "text"))))]}
    
@defform[(XML-ATTRS attr-expr ...)]{
Like @scheme[xml-attrs], but follows the same double-quoting rules as @scheme[XML].}

@defform[(opt-xml boolean-expr xml-expr ...)]{

Syntactic shorthand for:

@schemeblock[
  (if boolean-expr
      (xml xml-expr ...)
      (xml))]
      
@examples[
  #:eval xml-eval
  (xml->string (opt-xml #t (span "This span will be printed....")))
  (xml->string (opt-xml #f (span "...but this span won't.")))]}

@defform[(OPT-XML boolean-expr xml-expr ...)]{
Like @scheme[opt-xml], but follows the same double-quoting rules as @scheme[XML].}

@defform*[[(opt-xml-attr id)
           (opt-xml-attr boolean-expr id)
           (opt-xml-attr boolean-expr id expr)]]{
Syntactic shorthand for including an optional XML attribute. The full three-argument form expands to:

@schemeblock[
  (if boolean-expr
      (xml-attrs [id ,expr])
      (xml-attrs))]

The two- and one-argument forms are specialisations of the above. The two-argument form expands to:

@schemeblock[
  (if boolean-expr
      (xml-attrs [id ,id])
      (xml-attrs))]

and the one-argument form expands to:

@schemeblock[
  (if id
      (xml-attrs [id ,id])
      (xml-attrs))]
      
@examples[
  #:eval xml-eval
  (xml->string
   (xml ,@(for/list ([item (in-range 1 4)])
            (define class (if (even? item) "even" #f))
            (xml (li (\@ ,(opt-xml-attr class)) "Item " ,item)))))]}

@defform*[[(OPT-XML-ATTR id)
           (OPT-XML-ATTR boolean-expr id)
           (OPT-XML-ATTR boolean-expr id expr)]]{
Like @scheme[opt-xml-attr], but follows the same double-quoting rules as @scheme[XML].}

@section[#:tag "xml-expr"]{XML expression syntax}

The above forms the same XML syntax, inspired by the syntax of Neil van Dyke's @italic{HtmlPrag} package:

@schemegrammar*[
   #:literals (\@ & U !comment !cdata !pi !raw id xml attribute quotable-value symbol integer)
   [xml-expr          quotable-value
                      (custom-syntax-id xml-expr ...)
                      (tag xml-expr ...)
                      (tag (\@ attr-expr ...) xml-expr ...)
                      (& code-expr)
                      (!comment value-expr)
                      (!cdata value-expr)
                      (!pi value-expr)
                      (!raw value-expr)
                      (unquote xml)
                      (unquote-splicing (listof xml))]
    [custom-syntax-id (code:line id (code:comment "bound with define-xml-syntax"))]
    [tag              id]
    [code-expr        id
                      integer
                      (unquote (U symbol integer))]
    [value-expr       quotable-value
                      (unquote quotable-value)]
    [attr-expr        (id quotable-value)
                      (id (unquote quotable-value))
                      (unquote attribute)
                      (unquote-splicing (listof attribute))]]

@section{Custom XML syntax}

You can extend the XML language by introducing new syntax forms:

@defform*[[(define-xml-syntax (id arg ...) xml-expr)
           (define-xml-syntax id xml-transformer)
           (define-xml-syntax id xml-transformer expr-transformer)]]{
Defines a custom syntactic form for use in @scheme[xml] and @scheme[xml*] blocks.

The first form above behaves like @scheme[define-syntax-rule]. @scheme[xml-expr] should be a regular @scheme[xml] block.

@examples[
  #:eval xml-eval
  (define-xml-syntax (!linkto url)
    (xml (a (\@ [href url]) url)))
  (xml->string (xml (!linkto "http://www.plt-scheme.org")))
  (!linkto "http://www.plt-scheme.org")]
  
The second and third forms behave like @scheme[define-match-expander]. @scheme[xml-transformer] is a syntax transformer procedure used during XML expansion, which accepts an XML form as an input and returns a complete @scheme[xml] or @scheme[xml*] block representing the expansion. @scheme[expr-transformer] is a transformer procedure that is used in regular Scheme expansion. When @scheme[xml-transformer] is omitted, use of the syntax form outside of a XML block results in a syntax error.

@examples[
  #:eval xml-eval
  (define-xml-syntax !linkto
    (lambda (stx)
      (syntax-case stx ()
        [(_ url) #'(xml (a (\@ [href url]) url))]))
    (lambda (stx)
      (syntax-case stx ()
        [(_ url) #'url])))
  (xml->string (xml (!linkto "http://www.plt-scheme.org")))
  (!linkto "http://www.plt-scheme.org")]}
