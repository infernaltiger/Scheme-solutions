#lang Scheme
(define (f1 x1 y1 x2 y2)
(<=(+(sqr(- x1 x2)) (sqr(- y1 y2)))2)
  )
(define (f2 x1 y1 x2 y2)
  (<(- y1 y2)2))
(define (f3 x1 y1 x2 y2)
  (<(abs(- x1 x2))y2))
(define (f4 y2)
  (not(= y2 1)))
(define (F x1 y1 x2 y2)
  (and
   (f4 y2)
   (or
    (f1 x1 y1 x2 y2)
    (and(f3 x1 y1 x2 y2)(f2 x1 y1 x2 y2))
    )
   )
  )