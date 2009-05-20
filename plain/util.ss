#lang scheme/base

(require "../base.ss")

(require (for-syntax scheme/base)
         scheme/contract
         web-server/servlet)

; Syntax -----------------------------------------

; In or around PLT 4.1.4.3, the response? procedure was replaced with response/c.
; This hack allows one version of Mirrors to compile against PLTs before and after
; this change.

(define-syntax (web-server-response/c stx)
  (syntax-case stx ()
    [_ (cond [(identifier-binding #'response/c) #'response/c]
             [(identifier-binding #'response?)  #'response?]
             [else (error "response? and response/c not found")])]))

; Procedures -------------------------------------

; (listof header)
(define no-cache-http-headers
  (list (make-header #"Cache-Control" #"no-cache, no-store")
        (make-header #"Pragma"        #"no-cache")
        (make-header #"Expires"       #"Mon, 26 Jul 1997 05:00:00 GMT")))

; Provide statements -----------------------------

(provide web-server-response/c)

(provide/contract
 [no-cache-http-headers (listof header?)])
