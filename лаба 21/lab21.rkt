#lang Scheme
(define (f1 lst)
  (define (find x n lst)
    (if (equal? x (car lst)) n (find x (+ n 1) (cdr lst))))
  (define (it lst rez)
    (if (andmap (λ (x) (equal? x (list 'x))) lst) rez
        (let ((n (find null 0 lst)))
          (it (map (λ (x) (remove n x)) (list-set lst n (list 'x)))
              (cons n rez)))))
  (it lst  null))

(define (f2 graf lst_v)
  (define n (+ 1 (foldl (λ (p x) (apply max x p)) -1 graf)))
  (define N (build-list (if (= n 1) 2 n) +))
  (define (it graf N lst)
    (if (empty? lst) (cons graf N)
        (it (foldl (λ (p ng) (if (or
                                  (equal? (car lst) (car p))
                                  (equal? (car lst) (cadr p))) ng (cons p ng)))
                   null graf) (remove (car lst) N) (cdr lst))))
  (define (it2 n_graf n_N n)
    (if (empty? n_N) n_graf
        (it2
         (map (λ (p)
                (map (λ (x) (if (equal? x (car n_N)) n x)) p)) n_graf)
         (cdr n_N) (+ n 1))))
  
  (let* ((out (it graf N lst_v)))
    (it2 (car out) (cdr out) 0)))
     
(define (f3 lst)
  (define n (length lst))
  (define N (build-list n +))

  (define (it lst n rez)
    (if (empty? lst) (reverse rez)
        (let ((f (member n (car lst))))
        (it (cdr lst) (+ n 1)
            (cons (append (if f (list n) null)(foldl (λ (x l) (remove x l)) (remove n N) (car lst))) rez)
            ))))
  (it lst 0 null))

        
(define (f4 lst)
  (define N (build-list (length lst) +))
  (define (find x n lst)
    (if (equal? x (car lst)) n (find x (+ n 1) (cdr lst))))
  
  (define (it N stack tek rez)
    (if (empty? stack)
        (let ((nN (remove* tek N)))
          (if (empty? nN) (cons tek rez)
              (it nN (list (car nN)) (list (car nN)) (cons tek rez))
              ))
        (let ((next_v
               (findf (λ (x) (not (member x tek))) (list-ref lst (car stack)))))
          (if next_v
              (it N (cons next_v stack) (cons next_v tek) rez)
              (it N (cdr stack) tek rez)))))
  (it N (list (car N)) (list (car N)) null))



(define (f5 lst)

  (define (it queue prosm n)
    (if (empty? queue) (apply max (map (λ (x) (cdr x))n))
        (let ((next_s
               (filter (λ (x) (not (or (member x queue) (member x prosm))))
                       (list-ref lst (car queue)))))
          (if (empty? next_s)
              (it (cdr queue) prosm (cons (findf (λ (x) (equal? (car x)
                                                                (if (empty? (cdr queue))(car queue)(cadr queue)))) (cdr n)) n))
              (let ((n_q (append next_s queue)))
                (it n_q (cons (car n_q) prosm)
                    (append (build-list (length next_s)
                                        (λ (x) (cons (list-ref next_s x) (+ 1 (cdar n))))) n)))))))

  
 
(define (it2 N rez)
    (if (empty? N) rez
        (it2 (cdr N)
             (cons
              (cons (car N)
                    (it (list (car N)) (list (car N)) (list (cons (car N) 0))))
              rez))))
(define out (it2 (build-list (length lst) +) null))
(define radius (apply min (map (λ (x) (cdr x)) out)))
  (foldl (λ (par lst) (if (equal? radius (cdr par)) (cons (car par) lst) lst)) null out)
  )
              
        
