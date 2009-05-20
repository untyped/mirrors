#lang scheme/base

(require scheme/contract
         web-server/servlet
         "../plain/util.ss"
         "render.ss"
         "struct.ss")

;  [#:code      integer]
;  [#:message   string]
;  [#:seconds   integer]
;  [#:mime-type (U bytes string)]
;  [#:headers   (listof header)]
;  xml
; ->
;  response
(define (make-xml-response
         #:code      [code      200]
         #:message   [message   "OK"]
         #:seconds   [seconds   (current-seconds)]
         #:mime-type [mime-type #"text/xml; charset=utf-8"]
         #:headers   [headers   no-cache-http-headers]
         content)
  (let ([mime-type (if (bytes? mime-type)
                       mime-type
                       (string->bytes/utf-8 mime-type))])
    (make-response/full code message seconds mime-type headers (list (xml->string content)))))

;  [#:code      integer]
;  [#:message   string]
;  [#:seconds   integer]
;  [#:mime-type (U bytes string)]
;  [#:headers   (listof header)]
;  xml
; ->
;  response
(define (make-html-response
         #:code      [code      200]
         #:message   [message   "OK"]
         #:seconds   [seconds   (current-seconds)]
         #:mime-type [mime-type #"text/html; charset=utf-8"]
         #:headers   [headers   no-cache-http-headers]
         content)
  (let ([mime-type (if (bytes? mime-type)
                       mime-type
                       (string->bytes/utf-8 mime-type))])
    (make-response/full code message seconds mime-type headers (list (xml->string content)))))

(provide/contract
 [make-xml-response  (->* (xml?)
                          (#:code integer? 
                                  #:message   string?
                                  #:seconds   integer?
                                  #:mime-type (or/c bytes? string?)
                                  #:headers   (listof header?))
                          web-server-response/c)]
 [make-html-response (->* (xml?)
                          (#:code integer? 
                                  #:message   string?
                                  #:seconds   integer?
                                  #:mime-type (or/c bytes? string?)
                                  #:headers   (listof header?))
                          web-server-response/c)])
