#lang racket

(require (for-syntax syntax/parse))

; ;; SYNTAX
; ;; (define-function (f x ...) e)
; ;; binds f to a syntax tranformer of shape (cons n s)
; ;; where n is the arity |x ...| of f
; ;; and   s is syntax for (λ (x ...) e)
(define-syntax (define-function stx)
  (syntax-parse stx
    [(_ (f:id parameter:id ...) body:expr)
     (define arity (length (syntax->list #'(parameter ...))))
     #`(define-syntax f (cons #,arity #'(lambda (parameter ...) body)))]))

; ;; SYNTAX
; ;; (function-app f e1 ... eN)
; ;; applies f to the values of e1 ... IF f is defined and f's arity is N 
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

; Identifier Syntax -> (values N Id)
; EFFECT raises an exception if id is not available
(define-for-syntax (lookup id stx)
  ; -> Empty
  ; EFFECT abort process with syntax error 
  (define (failure)
    (define msg (format "undefined function: ~a" (syntax-e id)))
    (raise-syntax-error #f msg stx))
  (define result (syntax-local-value id failure))
  (values (car result) (cdr result)))

(define-syntax (plus stx)
  (syntax-parse stx
    ((_ x y) #`(cons #,2 #,#'(lambda (x y) (+ x y))))))

; (define-function (plus x y) (+ x y))
(function-app plus 2 3)