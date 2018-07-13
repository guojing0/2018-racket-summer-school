#lang racket

(require (for-syntax syntax/parse))
(require rackunit)

;;; Exercise 1

(define COUNTER -1)

(define-syntax (define-world* stx)
  (syntax-parse stx
    ((_ arg0:id)
     #'(begin
         (set! COUNTER (+ 1 COUNTER))
         (define arg0 COUNTER)
         (set! COUNTER -1)))
    ((_ arg0:id arg1:id ...)
     #'(begin
         (set! COUNTER (+ 1 COUNTER))
         (define arg0 COUNTER)
         (define-world* arg1 ...)))))

;; Tests for Exercise 1

(define-world* x y z)

(check-equal? x 0)
(check-equal? y 1)
(check-equal? z 2)

;;; Exercise 5

(define-syntax (define-rewrite-rule stx)
  (syntax-parse stx
    ((_ (name pattern ...) template)
     #'(define-syntax (name stx)
         (syntax-parse stx
           ((_ pattern ...) #'template))))))

;; Actual (generated) syntax

#;
(define-syntax (loop-for-ever stx)
  (syntax-parse stx
    ((_ exp)
     #'(local ((define (for-ever)
                 (begin exp (for-ever))))
         (for-ever)))))

(define-rewrite-rule ; Given example
  (loop-for-ever exp)
  ; —>
  (local ((define (for-ever)
            (begin exp (for-ever))))
    (for-ever)))

(loop-for-ever (displayln "hello"))
