#lang scribble/doc

@(require (file "base.ss"))

@(define-eval xml-eval (planet untyped/mirrors/xml/xml))

@title[#:tag "xml-util"]{XML utilities}

@defmodule[(planet untyped/mirrors/xml/util)]

The following XML expressions are defined for convenience:

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