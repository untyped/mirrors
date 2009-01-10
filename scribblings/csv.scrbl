#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "csv"]{CSV}

The CSV language in Mirrors allows the programatic assembly of syntactically valid @italic{Comma Separated Values (CSV)} data, most commonly used for interoperability with Microsoft Excel. Unlike the other languages in Mirrors, the CSV language is entirely procedure based and involves no custom syntax.

@include-section{csv-language.scrbl}
@include-section{csv-response.scrbl}
