#lang Scheme
(define % remainder)
(define // quotient)

(define (f x)
  (define (it d k)
    (if (= k 0) #t
        (if (= 0 d)
            #f
            (if (= 0 (% x d))
                (it (% (// k 10) 10) (// k 10))
                #f
                )
            )
        )
    )
  (if (= x 0) #f
      (it (% x 10) x)
      )
  )