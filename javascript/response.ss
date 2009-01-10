#lang scheme/base

(require scheme/contract
         web-server/servlet
         (file "../plain/util.ss")
         (file "render.ss")
         (file "struct.ss"))

;;  [#:code      integer]
;;  [#:message   string]
;;  [#:seconds   integer]
;;  [#:mime-type (U bytes string)]
;;  [#:headers   (listof header)]
;;  javascript
;; ->
;;  response
(define (make-js-response
         #:code      [code      200]
         #:message   [message   "OK"]
         #:seconds   [seconds   (current-seconds)]
         #:mime-type [mime-type #"text/javascript; charset=utf-8"]
         #:headers   [headers   no-cache-http-headers]
         content)
  (let ([mime-type (if (bytes? mime-type)
                       mime-type
                       (string->bytes/utf-8 mime-type))])
    (make-response/full code message seconds mime-type headers (list (javascript->string content)))))

; Provide statements -----------------------------

(provide/contract
 [make-js-response  (->* (javascript?)
                         (#:code integer? 
                                 #:message   string?
                                 #:seconds   integer?
                                 #:mime-type (or/c bytes? string?)
                                 #:headers   (listof header?))
                         response?)])
