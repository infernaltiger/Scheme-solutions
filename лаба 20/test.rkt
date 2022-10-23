#lang Scheme
(define (grAdjMatr->graph-edges graphAdjacencyMatriceFile)
  (define in (open-input-file graphAdjacencyMatriceFile))
  (define n (read in))
  (define stopPos (* n n))

  (define (iter pos edges)
    (define k (read in))
    (define src  (quotient  pos n))
    (define dest (remainder pos n))
    (if (= pos stopPos) edges
        (iter (add1 pos)
              (if (= k 0) edges
                  (foldl (lambda (n p) (cons (cons src dest) p)) edges (build-list k values))
                  )
              )
        )
    )
  (iter 0 null)
  )
