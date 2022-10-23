#lang Scheme
(define % remainder)
(define // quotient)

(define (f x)
  (define (it n k)
    (if (> x n)
        (it (* n 2) (+ k 1))
        (if (= x n) k #f)
        )
    )
  (it 1 0)
  )