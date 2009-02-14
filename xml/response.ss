#lang scheme/base

(require "../base.ss")

(require (unlib-in bytes)
         "../plain/util.ss"
         "render.ss"
         "struct.ss")

;  [#:code      integer]
;  [#:message   (U bytes string)]
;  [#:seconds   integer]
;  [#:mime-type (U bytes string)]
;  [#:headers   (listof header)]
;  xml
; ->
;  response
(define (make-xml-response
         #:code      [code      200]
         #:message   [message   #"OK"]
         #:seconds   [seconds   (current-seconds)]
         #:mime-type [mime-type #"text/xml; charset=utf-8"]
         #:headers   [headers   no-cache-http-headers]
         content)
  (make-response/full code
                      (ensure-bytes message)
                      seconds
                      (ensure-bytes mime-type)
                      headers
                      (list (string->bytes/utf-8 (xml->string content)))))

;  [#:code      integer]
;  [#:message   (U bytes string)]
;  [#:seconds   integer]
;  [#:mime-type (U bytes string)]
;  [#:headers   (listof header)]
;  xml
; ->
;  response
(define (make-html-response
         #:code      [code      200]
         #:message   [message   #"OK"]
         #:seconds   [seconds   (current-seconds)]
         #:mime-type [mime-type #"text/html; charset=utf-8"]
         #:headers   [headers   no-cache-http-headers]
         content)
  (make-response/full code
                      (ensure-bytes message)
                      seconds
                      (ensure-bytes mime-type)
                      headers
                      (list (string->bytes/utf-8 (xml->string content)))))

(provide/contract
 [make-xml-response  (->* (xml?)
                          (#:code integer? 
                                  #:message   (or/c bytes? string?)
                                  #:seconds   integer?
                                  #:mime-type (or/c bytes? string?)
                                  #:headers   (listof header?))
                          response/full?)]
 [make-html-response (->* (xml?)
                          (#:code integer? 
                                  #:message   (or/c bytes? string?)
                                  #:seconds   integer?
                                  #:mime-type (or/c bytes? string?)
                                  #:headers   (listof header?))
                          response/full?)])
