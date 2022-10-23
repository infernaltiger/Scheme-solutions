#lang Scheme
(define // quotient)
(define % remainder)
(define (f1 in_f)
  (define in (open-input-file in_f))
  (define n (read in))
  
  (define (read-skip n)
    (if (= n 0) (void)
        (begin (read in) (read-skip (- n 1)))))
  
  (define (it k1 k2 rez)
    (define x (read in))
    (cond
      ((equal? x eof) rez)
      ((and (not (= k1 n)) (= k2 (- n 1)))
       (it (+ 1 k1) 0 (+ rez (if (or (= x 0) (= k1 k2)) 1 0))))
      (else (if (or (= x 0) (= k1 k2))
                (it k1 (+ 1 k2) rez)
                (begin (read-skip (- n k2 1)) (it (+ k1 1) 0 rez))))))
   
  (it 0 0 0))


(define (f2 in_f)
  (define in (open-input-file in_f))
  (define n (read in))
  (define N (build-list n +))
  
  (define (it i)
    (define x (read in))
    (if (= n i) #t
        (if (= x (- n 1))
            (if (it2 x i) (it (+ 1 i)) #f)
            #f)))
  
  (define (it2 x i)
    (define (it x lst)
      (if (empty? lst) #t
          (if (= x 0) #f
              (it (- x 1) (remove (read in) lst)))))
    (it x (remove i N)))
  (it 0))

(define (f3 in_f out_f)
  (define in (open-input-file in_f))
  (define out (open-output-file out_f #:exists 'replace))
  (define n (read in))

  (define (it k)
    (define x (read in))
    (if (= k n) (close-output-port out)
        (begin (it2 x k) (it (+ k 1)))))
  (define (it2 n k)
    (if (= n 0) (void)
        (begin (display k out)
               (display #\space out)
               (display (read in) out)
               (newline out)
               (it2 (- n 1) k))))
    
  (begin (display n out) (newline out) (it 0)))

(define (f3_1 in_f out_f)
  (define in (open-input-file in_f))
  (define out (open-output-file out_f #:exists 'replace))
  (define n (read in))

  (define (it k lst)
    (define x (read in))
    (if (equal? x eof) (it2 (remove-duplicates lst
                                               (λ (a b)
                                            (and (not (equal? a b))(equal? a (cons (cdr b)(car b)))))))
        (it (+ k 1) (foldl (λ (p l) (cons (cons k p) l))
                           lst (build-list x (λ (a) (read in)))
                           ))))
  (define (it2 lst)
    (begin (display n out) (newline out)
           (for-each (λ (par) (display (car par) out)
                       (display #\space out)
                       (display (cdr par) out)
                       (newline out)
                       )
                     lst) (close-input-port in) (close-output-port out)))
    
  (it 0 null))



(define (f4 in_f)
  (define in (open-input-file in_f))
  (define n (read in))
  
  (define (it k lst)
    (define x (read in))
    (if (= k (* n n)) lst
        (it (+ k 1)
            (if (= x 0) lst
                (foldl
                 (λ (pass l) (cons (cons (// k n) (% k n)) l))
                 lst (build-list x +))))))
  (it 0 null)
  )

(define (f4_1 in_f)
  (define in (open-input-file in_f))
  (define n (read in))
  (define N (build-list n +))
  (define (it k lst)
    (if (= k n) (remove-duplicates lst
                                   (λ (a b)
                                     (and (not (equal? a b))(equal? a (cons (cdr b)(car b))))))
        (it (+ k 1)
            (foldl (λ (k1 k2 l) (if (= k1 0) l (append (build-list k1 ( λ (a)(cons k k2))) l)))
                   lst (build-list n (λ (a) (read in))) N
                   ))))
  
  (it 0 null))
  



(define (f5 in_f out_f)
  (define in (open-input-file in_f))
  (define out (open-output-file out_f #:exists 'replace))
  (define n (read in))
  (define pat (build-list n (λ (x) 0)))

  (define (it k lst)
    (define x (read in))
    (if (= k n) (it2 (reverse lst))
        (it (+ k 1)
            (cons
             (foldl (λ (x p) (let ((i (read in)))
                               (list-set p i (+ 1 (list-ref p i)))))
                    pat (build-list x +)) lst))))
  (define (it2 lst)
    (begin (display n out) (newline out)
           (for-each (λ (x)
                       (for-each
                        (λ (a) (display a out) (display #\space out)) x)
                       (newline out)) lst)
           (close-output-port out)))
  (it 0 null))
  
  




