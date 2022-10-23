#lang Scheme
(define % remainder)
(define // quotient)

(define (f x k)
  (define (it a k d)
    (if (or (>= 0 k) (= a 0)) (cons a (% x d))
        (it (// a 10) (- k 1) (* d 10))))
  (it x k 1)
  )