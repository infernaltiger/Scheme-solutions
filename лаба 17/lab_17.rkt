#lang Scheme
(define make-tree list)
(define data car)
(define left cadr)
(define right caddr)
(define (leaf? tree) (and (empty? (left tree)) (empty? (right tree))) )
;1 задача
(define (map-tree func tree)
  (if (empty? tree) null
      (make-tree
       (func (data tree)) (map-tree func (left tree)) (map-tree func (right tree)))))
;2 задача
(define (foldl-tree func iter tree)
  (if (empty? tree) iter
      
      (foldl-tree func (foldl-tree func (func (data tree) iter) (left tree)) (right tree))))
                  
;3 задача 
(define (f3 tree)
  (define (it tree)
    (cond ((empty? tree) null)
      ( (leaf? tree) tree)
      ( (not (or (empty? (left tree)) (empty? (right tree))))
        (make-tree (data tree) (it (left tree)) (it (right tree))))
      ( (empty? (left tree)) (it (right tree)))
      (else (it (left tree))))
    )
  (if (empty? tree) null (list (car tree) (it (left tree)) (it (right tree))))
  )
;4 задача

(define (f4 lst)
  (define (main_data lst)
    (ormap (lambda (x) (if (ormap (lambda (y) (equal? (car x) (cdr y))) (remove x lst)) #f x)) lst)
    )
  (define (it lst d_p)
    (if (empty? lst) null
        
        (let* (
               (lst1 (remove d_p lst))
               (kor (car d_p))
               (l (cdr d_p))
               (l_f (findf (λ (x) (equal? (car x) l)) lst1))
               (flag
                (findf (λ (x) (equal? (car x) kor)) lst1))
               (r (if flag (cdr flag) #f))
               (r_f (if r (findf (λ (x) (equal? (car x) r)) lst1) #f)))
          (make-tree
           kor
           (if l_f (it lst1 l_f) (list l null null))
        
          
           (if r_f (it lst1 r_f) (if r (list r null null) null))
           ))))
  (it lst (main_data lst)))
         
   

  
