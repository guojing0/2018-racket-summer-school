#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         syntax/parse/define)

;; Runtime

(struct theory (rules sols))
(define empty-theory (theory '() #f))
(define (theory-add thy new-rule)
  (match-define (theory rules sols) thy)
  (values (when sols
            "Theory changed; Dropping pending solutions")
          (theory (cons new-rule rules) #f)))
(define (theory-query thy query)
  (match-define (theory rules _) thy)
  (theory-next (theory rules (search-top rules query))))
(define (theory-next thy)
  (match-define (theory rules sols) thy)
  (match sols
    [#f (values "No active query" thy)]
    [(? stream-empty?) (values "No solutions" thy)]
    [s (values (stream-first s) (theory rules (stream-rest s)))]))
(provide empty-theory)

(define-syntax (relation stx)
  #'42)

(define-syntax (data stx)
  #'42)

(define-syntax (:- stx)
  #'42)

(define-syntax (? stx)
  #'42)

(define-syntax (next stx)
  #'42)

(define (teachlog-do!* thy-b tl-stmt)
  (define-values (result next-thy) (tl-stmt))
  (displayln result)
  (set-box! thy-b next-thy))

(define-syntax (teachlog-do! stx)
  (syntax-parse stx
    [(_ thy:id (~and e
                     (~or ((~literal relation) . _)
                          ((~literal data) . _))))
     #'e]
    [(_ thy:id (m . margs))
     #'(teachlog-do!* thy (lambda ()
                            (m (unbox thy) . margs)))]))

(define-simple-macro
  (teachlog (~optional (~seq #:theory init-thy:expr)) e ...)
  (let ([the-thy (box (~? init-thy empty-theory))])
    (teachlog-do! the-thy e) ...
    (unbox the-thy)))
