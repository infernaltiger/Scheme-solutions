#lang Scheme
(define (f x)
  (= 0  (foldl (λ(a y) (+ y (if (even? a) 0 1))) 0 x))
  )
(define (f2 x)
  (if (empty? x) #t
      (if (even? (car x)) (f2 (cdr x)) #f)))