#lang scribble/doc

@(require (file "base.ss"))

@(define-eval xml-eval (planet untyped/mirrors/xml/xml) (planet untyped/mirrors/xml/struct))

@title[#:tag "xml-ast"]{XML abstract syntax tree}

@defmodule[(planet untyped/mirrors/xml/struct)]

The @scheme[xml] and @scheme[xml*] macros expand into trees of structures of the following types. You shouldn't normally have to manipulate these types explicitly unless you want to build or manipulate XML without the syntax layer:

@defstruct[xml ()]{
XML data. This is an abstract type: concrete subtypes exist for the XML constructs such as elements, entities, CDATA sections and comments (see below).}

@defstruct[(atom xml) ([data quotable-value])]{
An atomic datum. The rendered form of an atom is simply its data, appropriately quoted so it cannot be considered markup:

@interaction[
  #:eval xml-eval
  (xml->string (make-atom 12345))
  (xml->string (make-atom #t))
  (xml->string (make-atom "Apples & pears."))]

@scheme[quotable-value]@schemeidfont{s} include booleans, numbers, strings, symbols, byte strings, SRFI 19 times (UTC and TAI types only) and URLs from the @scheme[net/url] library.}

@defstruct[(block xml) ([children (listof xml)])]{
A list of XML nodes. Blocks can be arbitrarily nested, and produce no markup other than that of their @scheme[children]:

@interaction[
  #:eval xml-eval
  (xml->string (make-block
                (list (make-atom "Apples ")
                      (make-atom "&")
                      (make-atom " pears."))))]}


@defstruct[(entity xml) ([code (U integer symbol)])]{
A character entity with a numeric or symbolic @scheme[code]:

@interaction[
  #:eval xml-eval
  (xml->string (make-entity 12345))
  (xml->string (make-entity 'nbsp))]}

@defstruct[(comment xml) ([data quotable-value])]{
A comment:

@interaction[
  #:eval xml-eval
  (xml->string (make-comment "Apples & pears"))]}

@defstruct[(cdata xml) ([data quotable-value])]{
A CDATA (unparsed character data) section:

@interaction[
  #:eval xml-eval
  (xml->string (make-cdata "Apples & pears"))]}

@defstruct[(pi xml) ([data quotable-value])]{
A processing instruction:

@interaction[
  #:eval xml-eval
  (xml->string (make-pi "Apples & pears"))]}

@defstruct[(raw xml) ([data quotable-value])]{
A block of raw XML, equivalent to a CDATA section without the opening and closing markup:

@interaction[
  #:eval xml-eval
  (xml->string (make-raw "<ul><li>Apples</li><li>Pears</li></ul>"))]}

@defstruct[(element xml) ([tag symbol] [attributes (listof attribute)] [child xml])]{
An XML element:

@interaction[
  #:eval xml-eval
  (xml->string (make-element
                'span
                (list (make-attribute 'title "attribute"))
                (make-atom "child element")))]}

@defstruct[attribute ([name symbol] [value quotable-value])]{
An element attribute.}
