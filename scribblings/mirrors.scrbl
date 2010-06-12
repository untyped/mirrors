#lang scribble/doc

@(require "base.ss")

@(require (for-label srfi/19
                     (only-in (planet bzlib/date-tz)
                              current-tz
                              tz-names)))

@title{@bold{Mirrors}: Programmatic Assembly of XML, Javascript and CSV Data}

Dave Gurnell

@tt{dave at @link["http://www.untyped.com"]{@tt{untyped}}}

@italic{Mirrors} is a collection of macro-based syntaxes for rendering content for web applications. The library currently supports the rendering of XML (including browser-compatible XHTML), Javascript 1.5 and CSV data. Future support is planned for CSS level 3.

@bold{Important: changes to time rendering in Mirrors 2.4:}

Mirrors 2.4 makes a subtle change in the way SRFI 19 times are rendered in XML and CSV output. The old behaviour was to use SRFI 19's @scheme[time-utc->date] and @scheme[time-tai->date] functions to convert the time to a @scheme[date], and then use @scheme[date->string] to render the date as a string.

The disadvantage of that approach is the original approach is that it always creates dates according to the @italic{current} time zone offset. This means that, for example, @scheme[time-utc]@schemeidfont{s} representing timestamps in the middle of winter, can be rendered using daylight saving time if your application is running in the middle of Summer.

Mirrors' new approach is to use the wrapped time/date handling functions from @scheme[(planet untyped/unlib/date)]. This module, which is essentially a wrapper for @scheme[(planet bzlib/date-tz)], converts times to dates using the immediate time zone for the current @italic{locale}. Winter times will always be rendered using a winter time zone offset, and summer times will always be rendered using a summer time zone offset.

The default locale is @scheme["GB"] but it can be overridden using the @scheme[current-tz] parameter. A complete list of locales can be obtained using the @scheme[tz-names] procedure. Both of these forms are provided by @scheme[(planet bzlib/date-tz)] and reprovided by @scheme[(planet untyped/unlib/date)].

@include-section{xml.scrbl}
@include-section{javascript.scrbl}
@include-section{csv.scrbl}
@include-section{plain.scrbl}

@section{Acknowledgements}

Many thanks to Dave Herman for @scheme[(planet dherman/javascript)] and @scheme[(planet dherman/pprint)], both of which are used extensively in Mirrors.

Thanks also to the following for their contributions: David Brooks, Matt Jadud, Fausto LS, Jay McCarthy, Karsten Patzwaldt and Noel Welsh.
