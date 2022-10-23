#lang Scheme
;Функция факториала
(define (fact y)
  (define (iter a b)
    (if (> b y)
        a
        (iter (* a b) (+ b 1))
        )
    )
  (iter 1 1))
;основная фунукия на итерации
(define (f x)
  (define (iter a b)
    (if (> a (fact b))
        (iter a (+ b 1))
        (= a (fact b))
        )
    )
    
  (iter x 1)
  )
