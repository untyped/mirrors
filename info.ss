#lang setup/infotab

(define name "mirrors")

(define blurb 
  '("Languages for the programatic assembly of XML, Javascript and CSV data."))

(define release-notes
  '((p "Changes and additions:")
    (ul (li "Mirrors is now simultaneously compatible with PLT versions older and newer than 4.1.3;")
        (li "added " (tt "!raw") " expressions and statements to the Javascript language;")
        (li "added uppercase versions of XML and JS quote equivalents (" (tt "xml") ", " (tt "opt-xml") ", " (tt "js") " and so on);")
        (li "using a lowercase quote form within a lowercase quoted block now results in a compile error;")
        (li "using an uppercase quote form within an uppercase quoted block now results in a compile error;")
        (li "removed extraneous newlines generated when pretty printing empty begin statements."))))

(define primary-file "mirrors.ss")

(define url "http://svn.untyped.com/mirrors/")

(define scribblings '(("scribblings/mirrors.scrbl" (multi-page))))

(define categories '(devtools net ui xml))

(define required-core-version "4.1.0.4")

(define repositories '("4.x"))

(define compile-omit-files '("sql"))


