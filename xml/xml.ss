#lang scheme/base

(require "expander.ss"
         "render.ss"
         "response.ss"
         "struct.ss"
         "syntax.ss"
         "util.ss")

(provide (all-from-out "render.ss"
                       "response.ss"
                       "syntax.ss"
                       "util.ss")
         define-xml-syntax
         xml?
         xml-quotable?
         xml+quotable?
         xml-quote
         xml-empty?
         #;xml-size
         #;xml-raw-size
         #;xml-depth
         #;xml-dump)

