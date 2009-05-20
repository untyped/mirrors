#lang scheme/base

(require "../base.ss")

(require web-server/servlet
         "../plain/util.ss"
         "render.ss"
         "struct.ss")

;  [#:code      integer]
;  [#:message   (U string bytes)]
;  [#:seconds   integer]
;  [#:mime-type (U string bytes)]
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
  (let ([message   (string+bytes->message   message)]
        [mime-type (string+bytes->mime-type mime-type)])
    (make-response/full code message seconds mime-type headers
                        (list (string+bytes->content (xml->string content))))))

;  [#:code      integer]
;  [#:message   (U string bytes)]
;  [#:seconds   integer]
;  [#:mime-type (U string bytes)]
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
  (let ([message   (string+bytes->message   message)]
        [mime-type (string+bytes->mime-type mime-type)])
    (make-response/full code message seconds mime-type headers
                        (list (string+bytes->content (xml->string content))))))

(provide/contract
 [make-xml-response  (->* (xml?)
                          (#:code integer? 
                                  #:message   (or/c string? bytes?)
                                  #:seconds   integer?
                                  #:mime-type (or/c string? bytes?)
                                  #:headers   (listof header?))
                          web-server-response/c)]
 [make-html-response (->* (xml?)
                          (#:code integer? 
                                  #:message   (or/c string? bytes?)
                                  #:seconds   integer?
                                  #:mime-type (or/c string? bytes?)
                                  #:headers   (listof header?))
                          web-server-response/c)])
