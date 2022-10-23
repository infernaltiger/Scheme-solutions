#lang Scheme
(define % remainder)
(define // quotient)

(define (kol x)
  (define (it x n)
    (if (= x 0) n (it (// x 10) (+ n 1)))
    ) (it (abs x) 0)
  )

(define (f x k)
  (let(
       (x (foldl (λ(a b) (if (<= (kol a) k) (cons a b) b)) null x))
       )
    (if (empty? x) #f
        (foldl (λ(a b) (* b a)) 1 x))
    )
  )
(define (f2 x k)
  (define (it x p f)
    (if (empty? x) (if (= 0 f) #f p)
        (if (<= (kol (car x)) k)
            (it (cdr x) (* p (car x)) (+ f 1))
            (it (cdr x) p f)
            )
        )) (it x 1 0)
  )
