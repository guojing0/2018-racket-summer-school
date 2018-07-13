#lang s-exp "type-checking-mb.rkt"

(lambda ([x : Int]) (+ x x))
10
#t
"hello"
(+ 1 (+ 2 3))
(if #t 98 3)
(if (if #t #f #t) 2 10)
(if (if #t #f #t) (if #t 32 0) (if #f 34 54))
