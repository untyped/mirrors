#lang scheme/base

(require scheme/contract
         web-server/servlet)

; (listof header)
(define no-cache-http-headers
  (list (make-header #"Cache-Control" #"no-cache, no-store")
        (make-header #"Pragma"        #"no-cache")
        (make-header #"Expires"       #"Mon, 26 Jul 1997 05:00:00 GMT")))

; Provide statements -----------------------------

(provide/contract
 [no-cache-http-headers (listof header?)])
