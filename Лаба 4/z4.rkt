#lang Scheme
(define (f x)
(flatten (cons (reverse (cdr x)) x))
  )
(define (f2 x)
  (define (it a b)
    (if (empty? a)  b (it (cdr a) (cons (car a) b)))
  ) (it (cdr x) x))