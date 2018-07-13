#lang s-exp "turnstile-main.rkt"
; e.g., save this to file "typed-lang-prog.rkt"
(require turnstile/rackunit-typechecking)
 
;(check-type 5 : Int)
;(check-type "five" : String)
; 
;(typecheck-fail #f #:with-msg "Unsupported literal")
;(typecheck-fail 1.1 #:with-msg "Unsupported literal")
; 
;(check-type + : (-> Int Int Int))
; 
;(check-type (+ 1 2) : Int -> 3)
; 
;(typecheck-fail (+ 1))

(if #t 3 2)