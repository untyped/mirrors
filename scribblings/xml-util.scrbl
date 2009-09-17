#lang scribble/doc

@(require (file "base.ss"))

@(define-eval xml-eval (planet untyped/mirrors/xml/xml))

@title[#:tag "xml-util"]{XML utilities}

@defproc[(xml-quotable? [val any]) boolean?]{
Returns @scheme[#t] if @scheme[val] is a Scheme value that can be converted to XML by Mirrors.

@examples[
  #:eval xml-eval
  (xml-quotable? 123)
  (xml-quotable? (xml 123))
  (xml-quotable? (box 123))]}

@defproc[(xml+quotable? [val any]) boolean?]{
Returns @scheme[#t] if @scheme[val] is an xml fragment or a Scheme value that can be converted to XML by Mirrors.

@examples[
  #:eval xml-eval
  (xml+quotable? 123)
  (xml+quotable? (xml 123))
  (xml+quotable? (box 123))]}

@defproc[(xml-quote [val (U xml? quotable-value?)]) xml?]{
Turns quotable values into xml fragments. Passes existing xml fragments straight through.

@examples[
  #:eval xml-eval
  (equal? (xml-quote 123) (xml ,123))
  (equal? (xml-quote (xml 123)) (xml 123))]}

@defproc[(xml-empty? [val xml?]) boolean?]{
Returns @scheme[#t] if @scheme[val] is an XML fragment that will produce no renderable XML source.
        
@examples[
  #:eval xml-eval
  (xml-empty? (xml))
  (xml-empty? (xml "Hi"))
  (xml-empty? (xml ,(xml)))
  (code:comment "Odd case: #f renders as an empty string:")
  (xml-empty? (xml #f)) ]}

@defmodule[(planet untyped/mirrors/xml/util)]

The following XML expressions are defined for convenience:

@defthing[xml-1.0-header xml]{
An XML 1.0 header that specifies the character encoding as UTF-8:

@interaction[
  #:eval xml-eval
 (display (xml->string xml-1.0-header))]}

@defthing[xhtml-1.0-strict-doctype xml]{
The HTML 1.0 transitional DOCTYPE, complete with newline character:

@interaction[
  #:eval xml-eval
 (display (xml->string xhtml-1.0-strict-doctype))]}

@defthing[xhtml-1.0-transitional-doctype xml]{
The HTML 1.0 transitional DOCTYPE, complete with newline character:

@interaction[
  #:eval xml-eval
 (display (xml->string xhtml-1.0-transitional-doctype))]}

@defthing[xhtml-1.0-frameset-doctype xml]{
The HTML 1.0 frameset DOCTYPE, complete with newline character:

@interaction[
  #:eval xml-eval
 (display (xml->string xhtml-1.0-frameset-doctype))]}
  
@defproc[(alist->attributes (alistof symbol? quotable-value?)) (listof attribute?)]{
Converts an association list to a list of attributes.}
