#lang setup/infotab

(define name "mirrors")

(define blurb 
  '("Languages for the programatic assembly of XML, Javascript and CSV data."))

(define release-notes
  '((p "Changes and additions:")
    (ul (li "replaced the " (tt "render-pretty-javascript?") " parameter with " (tt "javascript-rendering-mode") ";")
        (li "added " (tt "javascript->packed-string") ";")
        (li "fixed bug that disallowed named function declarations within " (tt "!begin") " blocks."))))

(define primary-file "mirrors.ss")

(define url "http://svn.untyped.com/mirrors/")

(define scribblings '(("scribblings/mirrors.scrbl" (multi-page))))

(define categories '(devtools net ui xml))

(define required-core-version "4.1.0.4")

(define repositories '("4.x"))

(define compile-omit-files '("sql"))


