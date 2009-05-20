#lang scheme/base

(require "../base.ss")

(require (for-syntax scheme/base)
         scheme/contract
         web-server/servlet
         (unlib-in string bytes))

; Syntax -----------------------------------------

; In or around PLT 4.1.4.3, two changes were made to the web server:
;   - the response? procedure was replaced with a contract, response/c;
;   - the message argument to make-response/foo became a bytes rather than a string.
;
; These macros allow Mirrors to compile and run with PLT versions prior to and after this change:

; (_ expr)
(define-syntax (string+bytes->message stx)
  (syntax-case stx ()
    [(_ arg ...)
     (cond [(identifier-binding #'response?)  #'(ensure-string arg ...)]
           [(identifier-binding #'response/c) #'(ensure-bytes arg ...)]
           [else (error "response? and response/c not found")])]
    [_ (cond [(identifier-binding #'response?)  #'ensure-string]
             [(identifier-binding #'response/c) #'ensure-bytes]
             [else (error "response? and response/c not found")])]))

; (_ expr)
(define-syntax (string+bytes->mime-type stx)
  (syntax-case stx ()
    [(_ arg ...)
     (cond [(identifier-binding #'response?)  #'(ensure-bytes arg ...)]
           [(identifier-binding #'response/c) #'(ensure-bytes arg ...)]
           [else (error "response? and response/c not found")])]
    [_ (cond [(identifier-binding #'response?)  #'ensure-bytes]
             [(identifier-binding #'response/c) #'ensure-bytes]
             [else (error "response? and response/c not found")])]))

; (_ expr)
(define-syntax (string+bytes->content stx)
  (syntax-case stx ()
    [(_ arg ...)
     (cond [(identifier-binding #'response?)  #'(ensure-string arg ...)]
           [(identifier-binding #'response/c) #'(ensure-bytes arg ...)]
           [else (error "response? and response/c not found")])]
    [_ (cond [(identifier-binding #'response?)  #'ensure-string]
             [(identifier-binding #'response/c) #'ensure-bytes]
             [else (error "response? and response/c not found")])]))

; Procedures -------------------------------------

; (listof header)
(define no-cache-http-headers
  (list (make-header #"Cache-Control" #"no-cache, no-store")
        (make-header #"Pragma"        #"no-cache")
        (make-header #"Expires"       #"Mon, 26 Jul 1997 05:00:00 GMT")))

; Provide statements -----------------------------

(provide string+bytes->message
         string+bytes->mime-type
         string+bytes->content)

(provide/contract
 [no-cache-http-headers (listof header?)])
