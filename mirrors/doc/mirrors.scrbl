#lang scribble/doc

@(require (file "base.ss"))

@title{Mirrors v1}

Dave Gurnell

@tt{dave at @link["http://www.untyped.com"]{@tt{untyped}}}

@italic{Mirrors} is a collection of macro-based syntaxes for rendering content for web applications. The library currently supports the rendering of XML (including browser-compatible XHTML) and Javascript 1.5, and future support is planned for CSS level 3.

@include-section{xml.scrbl}
@include-section{javascript.scrbl}
