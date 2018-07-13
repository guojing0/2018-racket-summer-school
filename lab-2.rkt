#lang racket

(require (for-syntax syntax/parse))

;;; Exercise 1

#;
(define-syntax (where stx) ; Matthew Flatt's first version
  (syntax-parse stx
    ((_ body:expr (name:id rhs:expr))
     #'(let ((name rhs)) body))))

(define-syntax (where stx) ; Matthew Flatt's second version
  (syntax-parse stx
    ((_ body:expr (name:id rhs:expr) ...)
     #'(let ((name rhs) ...) body))))

#;
(define-syntax (my-where stx) ; First version (not correct)
  (syntax-parse stx
    ((_ exp:expr binding:expr)
     #'(let (binding) exp))))

#;
(define-syntax (my-where stx) ; Second version (not correct)
  (syntax-parse stx
    ((_ exp:expr binding:expr ...)
     #'(let (binding ...) exp))))

;; Because
;;
;; (my-where 10 my-binding my-other-binding)
;; (my-where x (x 10) (x 5))

;; Examples

(where (+ my-favorite-number 2)
       [my-favorite-number 8])

(where (op 10 (+ my-favorite-number an-ok-number))
       [my-favorite-number 8]
       [an-ok-number 2]
       [op *])

(let ((my-fav-number 8) ; "Generated" code of "where"
      (an-ok-number 2)
      (op *))
  (op 10 (+ my-fav-number an-ok-number)))

(define-syntax (where* stx)
  (syntax-parse stx
    ((_ exp:expr (name:id rhs:expr) ...)
     #`(let* #,(reverse (syntax->list #'((name rhs) ...)))
         exp))))

;; Examples

(where* (list x y z)
        [x (+ y 4)]
        [y (+ z 2)]
        [z 1])

;;; Exercise 2

(define-syntax (and/v stx)
  (syntax-parse stx
    #:literals (=>)
    ((_ val:expr => var:id exp:expr ...)
     #'(let ((var val))
         (and var exp ...)))
    ((_ val exp ...)
     #'(and val exp ...))))

;; Examples

(and/v 1 => x (+ x 1))
(and/v 1 => x x)
(and/v #f => x (+ x 1))
(and/v 1 2 3)
(and/v #t #f)
