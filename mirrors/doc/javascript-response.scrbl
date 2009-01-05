#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "javascript-response"]{Rendering Javascript and sending Javascript responses}

@(declare-exporting (planet untyped/mirrors/javascript/javascript))

@section{Rendering Javascript in string form}

@defproc[(javascript->string [val javascript-statement?]) string?]{
Renders a Javascript statement as a compact string with no line breaks or indentation.}

@defproc[(javascript->pretty-string [val javascript-statement?]) string?]{
Renders a Javascript statement as a formatted string with line breaks and indentation.}

@section{Sending HTTP responses with Javascript content}

@defproc[(make-js-response [#:code code integer 200]
                           [#:message message string "OK"]
                           [#:seconds seconds integer (current-seconds)]
                           [#:mime-type mime-type (U string bytes) #"text/javascript; charset=utf-8"]
                           [#:headers headers (alistof symbol string) null]
                           [content javascript-statement]) response]{
Takes a @scheme[js] statement and wraps it in an HTTP response object that can be used with the PLT web server (including procedures such as @scheme[send/suspend] and @scheme[send/suspend/dispatch]). The keyword arguments correspond to the first five arguments of @scheme[make-response/full].}
