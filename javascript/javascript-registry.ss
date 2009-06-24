#lang typed-scheme

(require/opaque-type Javascript javascript? "javascript.ss")
(require/typed scheme/string [string-join ((Listof String) String -> String)])
(require/typed "javascript.ss" [javascript->string (Javascript -> String)])

(: registry (Listof Javascript))
(define registry null)

(: registry-add! (Javascript -> Void))
(define (registry-add! s)
  (set! registry (cons s registry)))

(: registry->string (-> String))
(define (registry->string)
  (string-join (map javascript->string (reverse registry)) "\n"))

(: registry-clear! (-> Void))
(define (registry-clear!)
  (set! registry null))

(provide
 registry-add!
 registry->string
 registry-clear!)
