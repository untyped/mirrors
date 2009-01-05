#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "js"]{Javascript}

The Javascript language in Mirrors allows the programatic assembly of syntactically valid Javascript. The language uses the AST structures and pretty printing libraries from Dave Herman's @tt{javascript.plt} package as its underlying representation.

@secref{javascript-syntax} describes the syntax for creating blocks of Javascript and @secref{javascript-response} describes how to send Javascript responses in the PLT web server.

@include-section{javascript-syntax.scrbl}
@include-section{javascript-response.scrbl}
