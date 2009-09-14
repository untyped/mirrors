#lang scribble/doc

@(require (file "base.ss"))

@(define-eval js-eval (for-syntax scheme/base) (planet untyped/mirrors/javascript/javascript))

@title[#:tag "javascript-response"]{Rendering Javascript and sending Javascript responses}

@(declare-exporting (planet untyped/mirrors/javascript/javascript))

@section{Rendering Javascript in string form}

@defproc[(javascript->string [val javascript-statement?]) string?]{
Renders a Javascript statement using the current @scheme[javascript-rendering-mode].}

@defproc[(javascript->pretty-string [val javascript-statement?]) string?]{
Renders a Javascript statement as a formatted string with line breaks and indentation.}

@defproc[(javascript->packed-string [val javascript-statement?]) string?]{
Renders a Javascript statement on a single line with no indentation.}

@defthing[javascript-rendering-mode (parameter (U 'pretty 'packed 'fast))]{
Affects the output of @scheme[javascript->string] and @scheme[javascript->pretty-string]. The possible values are:

@itemize{
  @item{@scheme['pretty] (the default value) - @scheme[javascript->string] renders Javascript as well formatted blocks;}
  @item{@scheme['packed] - @scheme[javascript->string] renders Javascript on a single line, with no indentation;}
  @item{@scheme['fast] - @scheme[javascript->string] uses an experimental renderer that writes all Javascript extremely quickly onto a single line.}}

The @scheme['fast] rendering mode is extremely naive: it inserts extra parentheses in expressions and extra semicolons between statements. At the time of writing, it has not been properly tested: use it with caution!

@examples[
  #:eval js-eval
  (define (display-statement)
    (display
     (javascript->string
      (js (function average (a b)
            (return (+ (/ a 2) (/ b 2))))))))
  (parameterize ([javascript-rendering-mode 'pretty])
    (display-statement))
  (parameterize ([javascript-rendering-mode 'packed])
    (display-statement))
  (parameterize ([javascript-rendering-mode 'fast])
    (display-statement))]}

@section{Sending HTTP responses with Javascript content}

@defproc[(make-javascript-response [#:code code integer 200]
                                   [#:message message string "OK"]
                                   [#:seconds seconds integer (current-seconds)]
                                   [#:mime-type mime-type (U string bytes) #"text/javascript; charset=utf-8"]
                                   [#:headers headers (alistof symbol string) no-cache-http-headers]
                                   [content javascript-statement]) response]{
Takes a @scheme[js] statement and wraps it in an HTTP response object that can be used with the PLT web server (including procedures such as @scheme[send/suspend] and @scheme[send/suspend/dispatch]). The keyword arguments correspond to the first five arguments of @scheme[make-response/full].}

@defproc[(make-js-response [#:code code integer 200]
                           [#:message message string "OK"]
                           [#:seconds seconds integer (current-seconds)]
                           [#:mime-type mime-type (U string bytes) #"text/javascript; charset=utf-8"]
                           [#:headers headers (alistof symbol string) no-cache-http-headers]
                           [content javascript-statement]) response]{
An alias for @scheme[make-javascript-response].}
