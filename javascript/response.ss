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
;  javascript
; ->
;  response
(define (make-javascript-response
         #:code      [code      200]
         #:message   [message   #"OK"]
         #:seconds   [seconds   (current-seconds)]
         #:mime-type [mime-type #"text/javascript; charset=utf-8"]
         #:headers   [headers   no-cache-http-headers]
         content)
  (let ([message   (string+bytes->message   message)]
        [mime-type (string+bytes->mime-type mime-type)])
    (make-response/full code message seconds mime-type headers
                        (list (string+bytes->content (javascript->string content))))))

; Contract as above.
(define make-js-response
  make-javascript-response)

; Provide statements -----------------------------

(provide/contract
 [make-javascript-response (->* (javascript?)
                                (#:code integer? 
                                        #:message   (or/c string? bytes?)
                                        #:seconds   integer?
                                        #:mime-type (or/c string? bytes?)
                                        #:headers   (listof header?))
                                response/full?)]
 [make-js-response         (->* (javascript?)
                                (#:code integer? 
                                        #:message   (or/c string? bytes?)
                                        #:seconds   integer?
                                        #:mime-type (or/c string? bytes?)
                                        #:headers   (listof header?))
                                response/full?)])
