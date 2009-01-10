#lang setup/infotab

(define name "mirrors")

(define blurb 
  '("Languages for the programatic assembly of XML, Javascript and CSV data."))

(define release-notes
  '((ul (li "added " (tt "define-xml-syntax") " and " (tt "define-javascript-syntax") ";")
        (li "added support for square-bracket references in " (tt "!dot") " expressions, e.g.:"
             (blockquote (tt "(!dot (!index a 1) (!index b 2) c)")))
        (li "fixed bug in catch statements with multiple substatements;")
        (li "moved to " (tt "javascript.plt 7") ","))))

(define primary-file
  "mirrors.ss")

(define url "http://svn.untyped.com/mirrors/")

(define scribblings '(("scribblings/mirrors.scrbl" (multi-page))))

(define categories '(devtools net ui xml))

(define required-core-version "4.0")

(define repositories '("4.x"))


