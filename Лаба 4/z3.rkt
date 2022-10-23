#lang Scheme
(define (f x)
  (define (it a x)
    (if (empty? x) a
        (if (>= a (car x)) a
            (it (car x) (cdr x)) 
            ))
    ) (it (car x) (cdr x))
  )
