#lang setup/infotab

(define name "mirrors")

(define blurb 
  '("Languages for the programatic assembly of XML, Javascript and CSV data."))

(define release-notes
  '((p "Changes this release:")
    (ul (li "added a new language for outputting CSV data;")
        (li "added " (tt "make-plain-response") " and " (tt "make-redirect-response") ";")
        (li "added " (tt "!regexp") " to the Javascript syntax;")
        (li "added " (tt "opt-xml") ", " (tt "opt-xml-attr") " and " (tt "opt-js") " macros "
            "for specifying optional XML and JS content;")
        (li "you can now use a plain " (tt "unquote") " to splice multiple attributes into an XML element "
            "(reducing the number of mistakes made with xml-attr and opt-xml-attr)."))))

(define primary-file
  "mirrors.ss")

(define url "http://svn.untyped.com/mirrors/")

(define scribblings '(("scribblings/mirrors.scrbl" (multi-page))))

(define categories '(devtools net ui xml))

(define required-core-version "4.0")

(define repositories '("4.x"))


