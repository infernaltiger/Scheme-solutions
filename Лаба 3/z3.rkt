#lang Scheme
(define (f a n)
  (define (it ak n li)
    (if (= n 1) li
        (if (even? ak)
            (it (/ ak 2) (- n 1) (cons (/ ak 2) li))
            (it (+ (* ak 3) 1) (- n 1) (cons (+ (* ak 3) 1) li))
            )

        )

    )
  (reverse (it a n (cons a null)))
  )
