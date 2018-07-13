#lang br/quicklang

(module+ reader
  (provide read-syntax))

(define (tokenize ip)
  (for/list ([tok (in-port read-char ip)])
    tok))

(define (parse toks)
  (letrec ([integer->tacos
            (lambda (n)
              (let ([div (quotient n 2)]
                    [rmd (modulo n 2)])
                (if (zero? div)
                    '(taco)
                    (cons (if (zero? rmd) '() 'taco)
                          (integer->tacos div)))))])
    (map (lambda (c)
           (integer->tacos (char->integer c)))
         toks)))

(define (read-syntax src ip)
  (define toks (tokenize ip))
  (define parse-tree (parse toks))
  (strip-context
   (with-syntax ([PT parse-tree])
     #'(module tacofied racket
         (for-each displayln 'PT)))))