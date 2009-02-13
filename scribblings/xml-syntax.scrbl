#lang scribble/doc

@(require (file "base.ss"))

@(define-eval xml-eval (for-syntax scheme/base) (planet untyped/mirrors/xml/xml))

@title[#:tag "xml-syntax"]{XML syntax}

@defmodule[(planet untyped/mirrors/xml/syntax)]

Mirrors proides two macros for building XML. Both of these forms have the same syntax, which is inspired by the list syntax used in Neil van Dyke's @italic{HtmlPrag} package:

@defform/subs[
  #:literals (\@ & U !comment !cdata !pi !raw id xml attribute quotable-value symbol integer)
  (xml xml-expr ...)
  ([xml-expr         quotable-value
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
   [custom-syntax-id id]
   [tag              id]
   [code-expr        id
                     integer
                     (unquote (U symbol integer))]
   [value-expr       quotable-value
                     (unquote quotable-value)]
   [attr-expr        (id quotable-value)
                     (id (unquote quotable-value))
                     (unquote attribute)
                     (unquote-splicing (listof attribute))])]{
Builds a block of XML, optimising any chunks of static markup into @scheme[raw] blocks.

@examples[
  #:eval xml-eval
  (let ([content "content"])
    (pretty-print (xml (span (\@ [title "title"]) ,content))))]
    
@scheme[custom-syntax-id] indicates an identifier bound with @scheme[define-xml-syntax].}
   
@defform[(xml* xml-expr ...)]{
The same as @scheme[xml] except that no markup optimisation is performed:

@interaction[
  #:eval xml-eval
  (let ([content "content"])
    (pretty-print (xml* (span (\@ [title "title"]) ,content))))]

It takes slightly longer to render the XML trees produced with @scheme[xml*] as strings, but the full structure of the tree is preserved.}

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

@defform[(xml-attrs attr-expr ...)]{
Builds a list of @scheme[attribute] structures. Useful in conjunction with the @scheme[unquote-splicing] form of @scheme[attr-expr]:

@interaction[
  #:eval xml-eval
  (let ([attrs (xml-attrs [title "title"] [href "href"])])
    (xml->string (xml (a (\@ ,@attrs) "text"))))]}

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
