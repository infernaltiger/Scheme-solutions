#lang Scheme
;целочисленное деление и остаток от деления(знаки как в питоне из-за привычки)
(define (// x y) (truncate (/ x y)))
(define (% x y) (remainder x y))
; основная функция на итерации
(define (f x)
  (define (it a b)
    (if (< a 10)
        (min a b)
        (it (// a 10) (min b (% a 10)))
        )
    )
  (it (abs x) 9)
  )
;на рекурсии
(define (f2 x)
  (if (< x 10)
      x
      (min (% x 10) (f2 (// x 10)))

      )
  )