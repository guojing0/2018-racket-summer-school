#lang turnstile/quicklang
; e.g., save this to file "typed-lang.rkt"
(provide Int String ->
         (rename-out [typed-app #%app]
                     [typed-+ +]
                     [typed-if if]
                     [typed-datum #%datum]))
 
(define-base-types Int String)
(define-type-constructor -> #:arity > 0)
 
(define-primop typed-+ + : (-> Int Int Int))

(define-primop typed-if if : (-> Bool τ τ))

#;
(define-typerule (typed-app f e ...) ≫
  [⊢ f ≫ f- ⇒ (~-> τin ... τout)]
  #:fail-unless (stx-length=? #'[τin ...] #'[e ...])
  (format "arity mismatch, expected ~a args, given ~a"
          (stx-length #'[τin ...])
          (stx-length #'[e ...]))
  [⊢ e ≫ e- ⇐ τin] ...
  --------------------
  [⊢ (#%plain-app f- e- ...) ⇒ τout])

(define-typerule typed-app
  [(_ f e ...)
   ≫
   [⊢ f ≫ f- ⇒ (~-> τin ... τout)]
   [⊢ e ≫ e- ⇐ τin] ...
   --------------------
   [⊢ (#%app f- e- ...) ⇒ τout]])

(define-typerule typed-datum
  [(_ . n:integer) ≫
   -------------
   [⊢ (#%datum . n) ⇒ Int]]
  [(_ . s:str) ≫
   -------------
   [⊢ (#%datum . s) ⇒ String]]
  [(_ . x) ≫
   --------
   [#:error (type-error #:src #'x #:msg "Unsupported literal: ~v" #'x)]])