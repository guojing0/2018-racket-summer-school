#lang br/quicklang

(provide #%module-begin)

(module+ reader
  (provide read-syntax))

(define (tokenize ip)
  (let [(datum (read ip))]
    (if (eof-object? datum)
        '()
        (cons datum (tokenize ip)))))

(define (parse tok)
  (if (list? tok)
      (map parse tok)
      'taco))

(define (read-syntax src ip)
  (define toks (tokenize ip))
  (define parse-tree (parse toks))
  (with-syntax ([(PT ...) parse-tree])
    #'(module tacofied racket
        'PT ...)))
