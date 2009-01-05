#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "csv-response"]{Rendering CSV data and sending CSV responses}

@defmodule[(planet untyped/mirrors/csv/csv-response)]

@section{Rendering CSV data in string form}

@defproc[(csv->string [val csv]) string]{
Renders a CSV sheet as a string.}

@defproc[(make-csv-response [#:code code integer 200]
                            [#:message message string "OK"]
                            [#:seconds seconds integer (current-seconds)]
                            [#:mime-type mime-type (U string bytes) #"text/xml; charset=utf-8"]
                            [#:headers headers (alistof symbol string) no-cache-http-headers]
                            [content csv]) response]{
Takes a @scheme[csv] content and wraps it in an HTTP response object that can be used with the PLT web server (including procedures such as @scheme[send/suspend] and @scheme[send/suspend/dispatch]). The keyword arguments correspond to the first five arguments of @scheme[make-response/full].}
