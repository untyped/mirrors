#lang scribble/doc

@(require (file "base.ss"))

@title[#:tag "xml-response"]{Rendering XML and sending XML responses}

@defmodule[(planet untyped/mirrors/xml/xml)]

@section{Rendering XML in string form}

@defproc[(xml->string [val xml?]) string?]{
Renders an XML item as a compact string, with no line breaks and no indentation.}

Mirrors does not contain a procedure for rendering XML as a multi-line string. This is largely because such a facility would be at odds with the pre-rendering behaviour of the @scheme[xml] macro. Please email the author if you feel this feature would be useful. If you want to debug the XML or XHTML output of our web application, you may want to try one of the variety of HTML and XML prettification add-ons that are available for Firefox.

@section{Sending HTTP responses with XML and XHTML content}

The PLT web server has built-in support for the ``@italic{xexpr}'' representation of the PLT @italic{xml} package. This lets you write request handling procedures in short-hand:

@schememod[
  scheme/base
  
  (require web-server/servlet
           xml/xml)

  (code:comment "request -> xexpr")
  (define (start initial-request)
    '(head (body "Hello world.")))]

instead of manually creating an HTTP response using @scheme[make-respone/full]:

@schememod[
  scheme/base
  
  (require web-server/servlet
           xml/xml)

  (code:comment "request -> response")
  (define (start initial-request)
    (make-response/full
     200
     "Okay"
     (current-seconds)
     #"text/html; charset=utf-8"
     null
     (list (xexpr->string '(head (body "Hello world."))))))]

Naturally, the PLT web server does not have built-in support for Mirrors XML expressions. Mirrors provides a couple of useful procedures to help you send responses:

@defproc[(make-xml-response [#:code code integer? 200]
                            [#:message message (U string? bytes?) #"OK"]
                            [#:seconds seconds integer? (current-seconds)]
                            [#:mime-type mime-type (U string? bytes?) #"text/xml; charset=utf-8"]
                            [#:headers headers (alistof symbol? string?) no-cache-http-headers]
                            [content xml?]) response/full?]{
Takes an @scheme[xml] expression argument and wraps it in an HTTP response object that can be used with the PLT web server (including procedures such as @scheme[send/suspend] and @scheme[send/suspend/dispatch]). The keyword arguments correspond to the first five arguments of @scheme[make-response/full].}

@defproc[(make-html-response [#:code code integer? 200]
                             [#:message message (U string? bytes?) #"OK"]
                             [#:seconds seconds integer? (current-seconds)]
                             [#:mime-type mime-type (U string? bytes?) #"text/html; charset=utf-8"]
                             [#:headers headers (alistof symbol? string?) no-cache-http-headers]
                             [content xml?]) response/full?]{
Like @scheme[make-xml-response] but with a default MIME type of @scheme{text/html}.}
