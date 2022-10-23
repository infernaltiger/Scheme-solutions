#lang Scheme
(define (f1 lst)
  (define (it lst)
    (if (>= 2 (length lst))
        (if (empty? lst) (list 0) (list (apply max lst) (apply max lst)))
        
        (let* ((lst1 (it (cddr lst)))
              (lst2 (it (cdddr lst)))
              
              (r1 (if (list? (car lst1)) 
                      (if (> (caar lst1) (caadr lst1)) (car lst1) (cadr lst1))
                      lst1))
                  
              
              (r2 (if (list? (car lst2)) 
                      (if (> (caar lst2) (caadr lst2)) (car lst2) (cadr lst2))
                      lst2)))
              
                    
          (list
           (cons (+ (car lst) (car r1)) (cons (car lst) (cdr r1)))
           (cons (+ (cadr lst) (car r2)) (cons (cadr lst) (cdr r2)))     
                      
                      ))))
    (define rez (it lst))
  (if (list? (car rez))
      (if (> (caar rez) (caadr rez)) (cdar rez) (cdadr rez))
      (apply max rez)
      ))

(define (f2 lst)
  
  (define (videlit x lst rez)
    (if (empty? lst) rez
        (if (> (car lst) x)
            (videlit (car lst) (cdr lst) (cons (car lst) rez))
            (videlit x (cdr lst) rez))))

  (define (it lst rez)
    (if (empty? lst) rez
        (let ((r (videlit (car lst) (cdr lst) (list (car lst)))))
          (it (remove* r lst) (cons r rez)))))
  
  (define (vivod lst k rez)
       (if (empty? lst) (cons (- k 1) (map cdr (sort rez (λ(a b)(< (car a) (car b))))))
           (vivod (cdr lst) (+ k 1) (append (foldl (λ(x l) (cons (cons x k) l)) null (car lst)) rez))))
  (vivod (it lst null) 1 null))


(define (f3 n lst id_t)
  ;'((2 3) (4 5 7) (6 9) () () (8 10) () () () ()) ->
  ; '((2 3) (1 4 5 7) (1 6 9) (2) (2) (3 8 10) (2) (6) (3) (6))
  (define (rem_orient n lst)
  (foldl (λ (node result) (foldl (λ (node1 new-lst)
                    (list-update new-lst (- node1 1)
                                 (λ (x)(if (member node x) x
                                                       (cons node x)))))
                  result (list-ref lst (- node 1))))
         lst (build-list n (λ (x) (+ 1 x)))))

  
  (define (dfs node1 node2 lst)
  (define path (sort (foldl (λ (node result)
                            (if (equal? node node2) result
                                (cons (dfs node node1 lst) result)))
                            '() (list-ref lst (- node1 1))) >))
  (define (iter time count res)
    
    (if (empty? time) res
        (if (> (+ count (car time)) res) (iter (cdr time) (+ count 1) (+ count (car time)))
            (iter (cdr time) (+ count 1) res))))
  (iter path 1 0))
  
  (dfs id_t #f (rem_orient n lst)))
  
      
  
  
    