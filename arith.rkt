#lang racket

(require (for-syntax syntax/parse))
 
(provide #%module-begin
         (rename-out [literal #%datum]
                     [plus +]
                     [arith-if if]
                     [arith-then then]
                     [arith-else else]
                     [complain-app #%app]))
 
(define-syntax (literal stx)
  (syntax-parse stx
    [(_ . v:number) #'(#%datum . v)]
    [(_ . v:string) #'(quote v)]
    [(_ . v:boolean) #'(#%datum . v)]
    [(_ . other) (raise-syntax-error #f "not allowed" #'other)]))
 
(define-syntax (plus stx)
  (syntax-parse stx
    [(_ n1 n2) #'(+ n1 n2)]))

(define-syntax (arith-if stx)
  (syntax-parse stx
    #:literals (arith-then arith-else)
    [(_ if-clause:expr arith-then then-clause:expr arith-else else-clause:expr)
     #'(if if-clause then-clause else-clause)]))

(define-syntax (arith-then stx)
  (raise-syntax-error 'then "improper use of then" stx))

(define-syntax (arith-else stx)
  (raise-syntax-error 'else "improper use of else" stx))

(define-syntax (complain-app stx)
  (define (complain msg src-stx)
    (raise-syntax-error 'parentheses msg src-stx))
  (define without-app-stx
    (syntax-parse stx [(_ e ...) (syntax/loc stx (e ...))]))
  (syntax-parse stx
    [(_)
     (complain "empty parentheses are not allowed" without-app-stx)]
    [(_ n:number)
     (complain "extra parentheses are not allowed around numbers" #'n)]
    [(_ x:id _ ...)
     (complain "unknown operator" #'x)]
    [_
     (complain "something is wrong here" without-app-stx)]))
