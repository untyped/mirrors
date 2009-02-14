#lang setup/infotab

(define name "mirrors")

(define blurb 
  '("Languages for the programatic assembly of XML, Javascript and CSV data."))

(define release-notes
  '((p "Changes:")
    (ul (li "improved syntax error messages in the XML and Javascript languages;")
        (li "added checks for badly formed tag and attribute names in the XML language."))))

(define primary-file
  "mirrors.ss")

(define url "http://svn.untyped.com/mirrors/")

(define scribblings '(("scribblings/mirrors.scrbl" (multi-page))))

(define categories '(devtools net ui xml))

(define required-core-version "4.1.3")

(define repositories '("4.x"))


