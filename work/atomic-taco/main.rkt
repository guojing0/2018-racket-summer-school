#lang racket

(require (for-syntax syntax/parse))

(provide #%module-begin
         #%top-interaction
         (rename-out [tacos #%datum]
                     [tacos #%top]
                     [tacos-app #%app]))

(module reader syntax/module-reader
  atomic-taco)

(define-syntax (tacos stx)
  (syntax-parse stx
    [(_ . var) #'(#%datum . taco)]))

(define-syntax (tacos-app stx)
  (syntax-parse stx
    [(_ function:id args:expr ...) #'`(tacos ,args ...)]))