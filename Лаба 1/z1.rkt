#lang Scheme
(define (lab1.1 a x y)
  (cond ((>= y 0) (abs (- (sqrt (+ (* x x) (* y y))) (abs a))))
        ((< y 0) (if
                  (and
                   (>= (abs y) (abs a))
                   (>= (abs x) (abs a))
                   )
                  (sqrt (+
                         (sqr (- (abs x) (abs a)))
                         (sqr (- (abs y) (abs a)))
                         )
                  )
                  (if (and (<= (abs x) (abs a)) (> (abs y) (abs a)))
                      (- (abs y) (abs a))
                  (min
                   (abs (- (abs x) (abs a)))
                   (abs (- (abs y) (abs a)))
                   )
                  ))
    )
 ))