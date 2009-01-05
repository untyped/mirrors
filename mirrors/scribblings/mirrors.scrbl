#lang scribble/doc

@(require (file "base.ss"))

@title{@bold{Mirrors}: Programmatic Assembly of XML, Javascript and CSV Data}

Dave Gurnell

@tt{dave at @link["http://www.untyped.com"]{@tt{untyped}}}

@italic{Mirrors} is a collection of macro-based syntaxes for rendering content for web applications. The library currently supports the rendering of XML (including browser-compatible XHTML), Javascript 1.5 and CSV data. Future support is planned for CSS level 3.

@include-section{xml.scrbl}
@include-section{javascript.scrbl}
@include-section{csv.scrbl}
@include-section{plain.scrbl}

@section{Acknowledgements}

Mirrors makes heavy use of Javascript.plt and PPrint.plt by Dave Herman. Many thanks to Dave for all of his hard work.

Thanks also to David Brooks, Matt Jadud, Fausto LS, Jay McCarthy, Noel Welsh, and for their contributions, feedback, suggestions and bug fixes.
