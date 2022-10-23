#lang Scheme
(define (// a b) (truncate (/ a b)))
(define (ch n) (= n (* 2 (// n 2))))
(define (=co x1 y1 x2 y2) (not (xor (ch (+ x1 y1)) (ch (+ x2 y2)))))
(define (d1 x1 y1  x2 y2) (= (abs (- x1 x2)) (abs (- y1 y2))))
(define (spp x1 y1 x2 y2) (if (=co x1 y1 x2 y2) (if (d1 x1 y1 x2 y2) 1 2) "нет ходов"))