#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "xml"]{XML}

The XML language in mirrors allows the programatically assembly of syntactically correct XML and XHTML, without the problems of traditional list-representations:

@itemize{
  @item{the data type of an @scheme[element] is distinct from the data type of a @scheme[block] of nodes;}
  @item{blocks can be arbitrarily nested without using forms such as @scheme[append], @scheme[quasiquote] and @scheme[unquote-splicing];}
  @item{@italic{raw} blocks of unprocessed text can be inserted into an otherwise well-formed XML document;}
  @item{many XHTML browser bugs are automatically worked around;}
  @item{blocks of Javascript are automatically rendered as appropriately quoted attribute values and text;}   
  @item{static blocks of output are pre-rendered to strings at macro expansion time.}}

@secref{xml-syntax} describes the syntax for creating blocks of XML, @secref{xml-response} describes how to send XML responses in the PLT web server, @secref{xml-util} describes some useful blocks of XML such as a DOCTYPES, and @secref{xml-ast} describes the underlying AST representation.

@include-section{xml-syntax.scrbl}
@include-section{xml-response.scrbl}
@include-section{xml-util.scrbl}
@include-section{xml-ast.scrbl}
