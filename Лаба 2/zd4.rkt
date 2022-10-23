#lang Scheme
;функция числа фибоначи
(define (fib x)
  (define (iter a b k)
    ( if (= k 0)
         b
         (iter b (+ a b) (- k 1)))
    )
  (iter 1 1 (- x 1))
  )
; основная функция
(define (f x)
  (define (it a b)
    (let
        (
         (q (fib a))
         (w (fib b))
         )
      ( if (>= w x)   
           (if ( > (abs (- x q)) (abs (- x w))) w q)
           (it (+ a 1) (+ b 1))
           )
      )
    )
  (it 1 2)
  )
; на основе практики более оптимизированно
(define (f2 x)
  (define (it a b)
    ( if (>= b x)
         (if ( > (abs (- x a)) (abs (- x b))) b a)
         (it b (+ b a))
         )
    )
  (it 0 1)
  )
