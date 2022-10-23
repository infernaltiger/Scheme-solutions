#lang Scheme
(define (f1 lst)
  (define n (length lst))
  (define (it old n new)
    (if (empty? old) new
        (it (cdr old) (+ n 1) (foldl (λ (r lst)
                                       (list-set lst r (cons n (list-ref lst r)))) new (car old)))))
  (it lst 0 (build-list n (λ (x ) null))))

(define (find x n lst)
    (if (empty? lst) #f
        (if (equal? x (car lst)) n
            (find x n (cdr lst)))))
    
(define (f2 g)
  
  (define (find x n lst)
    (if (empty? lst) #f
        (if (equal? x (car lst)) n
            (find x n (cdr lst)))))
    
  (define (it lst n res)
    (if (empty? lst) (remove-duplicates res)
        (if (> 2 (length (car lst))) (it (cdr lst) (+ n 1) res)
            (let ((trios (foldl (λ (i res)
                                  (let ((path (foldl
                                               (λ (j l) (cons (find i j (list-ref g j)) l))
                                              null (remove i (car lst)))))
                                    (if (not (member #f path)) (append (foldl (λ (x y) (cons (sort (list n i x) <) y))null path) res) res)))
                                res (car lst))))
              (it (cdr lst) (+ n 1)  trios)))))
  (it g 0 null))

(define make-tree list)
(define data car)
(define left cadr)
(define right caddr)
(define (leaf? tree) (and (empty? (left tree)) (empty? (right tree))) )
(define (map-tree func tree)
  (if (empty? tree) null
      (make-tree
       (func (data tree)) (map-tree func (left tree)) (map-tree func (right tree)))))

(define (foldl-tree func iter tree)
  (if (empty? tree) iter
      
      (foldl-tree func (foldl-tree func (func (data tree) iter) (left tree)) (right tree))))

(define (f3 tree)
  (define N (foldl-tree (λ (x lst) (cons (cons x (length lst))lst)) null tree))
  (define n_t (map-tree (λ (x) (cdr (findf (λ (y) (equal? x (car y))) N))) tree))
  
  
 
  (define (it tree  g)
    (if (empty? tree) g 
        (let ((new_str (append
                        (if (empty? (left tree)) null (list (data(left tree))))
                        (if (empty? (right tree)) null (list (data (right tree)))))))
             
          (it (right tree)  (it (left tree) (cons new_str g))))))

  (define (it2 lst n rez N)
    (if (= n N) rez
        (it2 lst (+ n 1)
           (list-set rez n (append (list-ref rez n) (foldl (λ (l nom perem)
                       (append (if (member n l) (list nom) null) perem)) null lst (build-list N +)))) N)))

  (define out1 (reverse (it n_t null)))
  (define out2 (it2 out1 0 out1 (length out1)))
  (list N out2))

(define (f4 lst)
  (define n (length lst))
  (define N (build-list n +))
  
  (define (it stack pos)
    (if (empty? stack) (equal? (sort pos <) N)
        (if (> (length (list-ref lst (car stack))) 3) #f
        (let ((next (findf (λ (x) (not(member x pos))) (list-ref lst (car stack)))))
          (if (and (not (empty? (cdr stack)))
                   (ormap (λ(x) (member x stack) ) (remove (cadr stack)(list-ref lst (car stack))))) #f
          (if next
              (it (cons next stack) (cons next pos))
              (it (cdr stack) pos)))))))

  (define (find_mroot lst n)
    (if (>= 2 (length (car lst))) n (find_mroot (cdr lst) (+ n 1))))
  (define (it2 tek main_r)
    (if (empty? tek) (make-tree main_r null null)
        (let*((f1 (empty? tek))
              (left-t (if f1 null (car tek)))
              (f2 (empty? (cdr tek)))
              (right-t (if f2 null (cadr tek))))
          (make-tree main_r
             (if f1 null (it2 (remove main_r (list-ref lst left-t)) left-t))
             (if f2 null (it2 (remove main_r (list-ref lst right-t)) right-t))))))
          

  (if (empty? lst) #f (if (it (list 0) (list 0))
      (let ((mr (find_mroot lst 0)))
        (it2 (list-ref lst mr) mr))
      #f)))
 
  
  

  

