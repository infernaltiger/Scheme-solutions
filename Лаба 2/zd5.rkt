#lang Scheme
;целочисленное деление и остаток от деления(знаки как в питоне из-за привычки)
(define // quotient)
(define % remainder)
;проверка x делится на y(возвращаем число чтобы сдеелать сумму)
(define (del x y)
  (if
   (and
    (not(= x y))
    (= (// x y) (/ x y))
    )
   y
   0
   )
  )
;сумма делителей без самого числа
(define (sum_del x)
  (define (it a b)
    (if (> b (+ 1 (/ a 2)))
        0
        (+ (del a b) (it a (+ b 1)))
        )
    )
  (it x 1)
  )
;основная функция
(define (f x)
  (= x (sum_del x)
     )
  )
; 2 способ более оптимизированный на основе чужей идии
(define (f2 x)
  (define (it a b)
    (if (= x b)
        #t
        (if (< (// x 2) a)
            #f
            (if (= (% x a) 0)
                (it (+ a 1) (+ a b))
                (it (+ a 1) b)
                )
            )
        ))
  (it 1 0)
  )
;3 способ на основе идеи о корне
(define (f3 x)
  (define (it s d)
    (if (>= x (sqr d))
        (if (= 0 (% x d))
            (if (= (sqr d) x)
                (it (+ s d) (+ d 1))
                (it (+ s d (// x d)) (+ d 1)))
            (it s (+ d 1)))
        s))
  (and (= x (it 1 2)) (> x 1))
        )
    