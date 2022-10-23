#lang Scheme
;задание 1 (z1 '(3 4 5 7 9 1))
(define (z1 sequence)
  (define (iter lst sum result)
    (if (empty? lst) (car result)
        (let* ((x (car lst))
               (sum1 (+ x (cadr sum))) ;sum1 - максимальная сумма с рассматриваемым элементом
               (sum2 (car sum))) ;sum2 - максимальная сумма без рассматриваемого элемента
          (if (> sum1 sum2) (iter (cdr lst) (cons sum1 sum) (cons (cons x (cadr result)) result))
              (iter (cdr lst) (cons sum2 sum) (cons (car result) result))))))
  (iter sequence '(0 0) '(() ())))

;задание 2 (z2 '(3 1 2))
(define (split lst)
(define (bin-search x quantity stack)
  (define (iter l r)
    (if (> l r) l
       (let ((c (quotient (+ l r) 2)))
          (if (> (car (list-ref stack c)) x)
              (iter (+ c 1) r)
              (iter l (- c 1))))))
  (iter 0 (- quantity 1)))
  
  (define (make-stack lst quantity stack)
    (if (empty? lst) stack
        (let* (
               (k (bin-search (car lst) quantity stack)))
          (if (= k quantity) (make-stack (cdr lst) (+ 1 quantity) (append stack (list (list (car lst)))))
              (make-stack (cdr lst) quantity (list-update stack
                                                            k
                                                            (lambda (st) (cons (car lst) st))))))))
    (make-stack lst 0 '()))

(define (z2 lst)
  (define splited (split lst))
  (define quantity (length splited))
  (define (f x splits result)
    (if (= x 0) (cons quantity result)
        (f (- x 1) (cdr splits) (foldl (lambda (item res)
                                         (list-set res (- item 1) x)) result (car splits)))))
    (f quantity splited (build-list (length lst) values)))
                                      
  
;задание 3 (z3 10 '((2 3) (4 5 7) (6 9) () () (8 10) () () () ()) 2)

(define (DFS node1 node2 lst)
  (define path (sort (foldl (lambda (node result)
                            (if (equal? node node2) result
                                (cons (DFS node node1 lst) result)))
                            '() (list-ref lst (- node1 1))) >))
  (define (iter time call result)
    (if (empty? time) result
        (if (> (+ call (car time)) result) (iter (cdr time) (+ call 1) (+ call (car time)))
            (iter (cdr time) (+ call 1) result))))
  (iter path 1 0))

(define (drop-orientation n lst)
  (foldl (lambda (node result)
           (foldl (lambda (node1 new-lst)
                    (list-update new-lst (- node1 1)
                                 (lambda (x)(if (member node x) x
                                                       (cons node x)))))
                                          
                  result (list-ref lst (- node 1))))
         lst (build-list n (lambda (x) (+ 1 x)))))

(define (z3 n lst ID)
  (DFS ID #f (drop-orientation n lst)))
  



