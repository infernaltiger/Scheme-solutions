#lang Scheme
;целочисленное деление и остаток от деления(знаки как в питоне из-за привычки)
(define // quotient)
(define % remainder)
; основная функция
(define (f x)
  (define (it a b)
    (if (= a 0)
        #t
        (if (= (% a 10) b)
            (it (// a 10) b)
            #f
            )
        )
    )
  (it x (% x 10))
  )