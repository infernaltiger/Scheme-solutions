#lang Scheme
(define % remainder)
(define // quotient)

(define (f x)
  (define (it d n)
    (if (>= x (sqr d))
        (if (and(= 0 (% x d))(<= (sqr d) x) (not(= x (* d d d)) ))
            (it (+ 1 d) (+ 2 n))
            (it (+ 1 d) n)
            )
        (if (= n 2) #t #f)
    
        ))
  (it 2 0)
  )