#lang racket

(require (for-syntax syntax/parse))

(provide #%module-begin
         (rename-out [literal #%datum]
                     [algebra-if if]
                     [plus +]))

(module reader syntax/module-reader
  algebra
  #:wrapper1 (lambda (t)
               (parameterize ([read-decimal-as-inexact #f])
                 (t))))

(define-syntax (literal stx)
  (syntax-parse stx
    [(_ . v:number) #`(#%datum . #,(exact->inexact (syntax-e #'v)))]
    [(_ . v:string) #'(quote v)]
    [(_ . v:boolean) #'(#%datum . v)]
    [(_ . other) (raise-syntax-error #f "not allowed" #'other)]))

;; SYNTAX
;; (define-function (f x ...) e)
;; binds f to a syntax tranformer of shape (cons n s)
;; where n is the arity |x ...| of f
;; and   s is syntax for (λ (x ...) e)
(define-syntax (define-function stx)
  (syntax-parse stx
    [(_ (f:id parameter:id ...) body:expr)
     (define arity (length (syntax->list #'(parameter ...))))
     #`(define-syntax f (cons #,arity #'(lambda (parameter ...) body)))]))

;; SYNTAX
;; (function-app f e1 ... eN)
;; applies f to the values of e1 ... IF f is defined and f's arity is N 
(define-syntax (function-app stx)
  (syntax-parse stx
    [(_ f:id arg:expr ...)
     (define n-args (length (syntax->list #'(arg ...))))
     (define-values (arity the-function) (lookup #'f stx))
     (cond
       [(= arity n-args) #`(#,the-function arg ...)]
       [else
        (define msg (format "wrong number of arguments for ~a" (syntax-e #'f)))
        (raise-syntax-error #f msg stx)])]))

;; Identifier Syntax -> (values N Id)
;; EFFECT raises an exception if id is not available
(define-for-syntax (lookup id stx)
  ; -> Empty
  ; EFFECT abort process with syntax error 
  (define (failure)
    (define msg (format "undefined function: ~a" (syntax-e id)))
    (raise-syntax-error #f msg stx))
  (define result (syntax-local-value id failure))
  (values (car result) (cdr result)))

(define-syntax (algebra-if stx)
  (syntax-parse stx
    [(_ if-clause then-clause else-clause)
     #'(if if-clause then-clause else-clause)]))

(define-syntax (plus stx)
  (syntax-parse stx
    [(_ x y) #'(+ x y)]))

(define-syntax (exactly stx)
  (syntax-parse stx
    [(_ x) #'(inexact->exact x)]))

(define-syntax (inexactly stx)
  (syntax-parse stx
    [(_ x) #'(exact->inexact x)]))

(define-syntax (algebra-div stx)
  (syntax-parse stx
    [(_ x y) #'()]))
