#lang Scheme

(define (f1 lst)
  
  (define (sum-gcd lst sum)
    (if (empty? (cdr lst)) sum
        (sum-gcd (cdr lst) (+ sum (gcd (car lst) (cadr lst))))))

  (define (it lst rez)
    (if (empty? lst) rez
        (foldl (λ (x l)
                 (let ((r (it (remove x lst) (cons x rez))))
                   (if (> (sum-gcd r 0) (sum-gcd l 0)) r l)))
               lst lst)))

  (it lst null))
(define (f2 lst)
  
  (define (sum-dist-centr lst)
    (define (Tr m)
    (apply map list m))
    
    (define t_l (Tr lst))
    (define centr (list
                   (/ (apply + (car t_l)) (length (car t_l)))
                   (/ (apply + (cadr t_l)) (length (cadr t_l)))
                   ))
    (define (rast a b)
      (+
       (*(- (car a) (car b)) (- (car a) (car b)))
       (*(- (cadr a) (cadr b)) (- (cadr a) (cadr b)))))
    (define (it lst sum)
      (if (empty? lst) sum
          (it (cdr lst) (+ sum (rast (car lst) centr)))))
    (it lst 0))
  
 
    
    (define (it lst_tek lst_rem rez)
    
    (if (>= 1 (length lst_rem))
        (append (list (cons (sum-dist-centr lst_rem) lst_rem)) rez)
        
            (append (list (cons (sum-dist-centr lst_rem) lst_rem))
                    (foldl (λ (x l) (append (it (remove x lst_rem) (remove x lst_rem) rez) l)) null lst_tek)
                    rez)))
 
  
   (define out (remove-duplicates (it lst lst null)))
  (define maxim (car (foldl (λ (x lst) (if (> (car x) (car lst)) x lst))  (car out) out)))
  (filter (λ (x) (equal? (car x) maxim)) out))

(define (f3 n lst)
  (define (it n lst rez)
    (define l_f (filter (λ (x)(> (gcd n x) 1)) lst))
       (if (empty? l_f) rez
           (foldl (λ (x l)
                    (let ((r (it x (remove x lst) (cons x rez))))
                      (if (> (length r) (length l)) r l)))
                  rez l_f)))
  (reverse (it n lst (list n))))
                                  
         
   

  