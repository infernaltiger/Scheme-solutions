#lang Scheme

(define (f1 lst n)
  (define l_n (length lst))
  (define (it1 lst z o t)
    (if (empty? lst) (list z o t)
        (cond ((= 0 (remainder (car lst) 3)) (it1 (cdr lst) (cons (car lst) z) o t))
              ((= 1 (remainder (car lst) 3)) (it1 (cdr lst) z (cons (car lst) o) t))
              ((= 2 (remainder (car lst) 3)) (it1 (cdr lst) z o (cons (car lst) t)))
              )
        )
    )
  (define (it2 n z o t rez)
    (if (= 0 n) rez
        (if (or (empty? z) (empty? o) (empty? t)) #f
            (it2 (- n 1)
                 (cdr z) (cdr o) (cdr t)
                 (cons (list (car z) (car o) (car t)) rez))
            )
        
        )
    )
  (if (> (* 3 n) l_n) #f
      (let ((trio (it1 lst null null null)))
        (if (or (empty? (car trio)) (empty? (cadr trio)) (empty? (caddr trio)))
            #f
            (it2 n (car trio) (cadr trio) (caddr trio) null))
        )
      )
  )



(define (f2 lst)
  (define (it lst catal fl)
    (if (empty? lst) catal
        (let ((pom (string->list (car lst))))
          (if (and (member #\\ pom) (member #\: pom))
              (let ((otv (list->string (remove #\\ pom))))
                (it (cdr lst) (if (empty? catal) (list otv)
                                  (if (equal? (car (reverse catal)) otv)
                                      catal (list otv))) #t))
          (it (cdr lst) (if fl (cons (list->string (cons #\\ pom)) catal) catal) fl)))))
(define (it2 lst rez)
  (if (empty? lst) (apply string-append rez)
      (if (and (not (empty? rez)) (not (empty? (cdr rez))) (equal? (car lst) (cadr rez)))
          (it2 (cdr lst) (cdr rez))
          (it2 (cdr lst)
               (cons (car lst) rez)))))


(let ((it_rez (it lst null #f)))
  (if (empty? it_rez) #f (it2 it_rez null))))











(define make-tree list)
(define data car)
(define left cadr)
(define right caddr)
(define (leaf? tree) (and (empty? (left tree)) (empty? (right tree))) )
(define (tree->list tree)
  (define (iter tree result-list)
    (if (empty? tree) result-list
        (iter (left tree)
              (cons (data  tree)
                    (iter (right tree)
                          result-list)
                    )
              )
        )
    )
  (iter tree null)
  )




(define (f3 tree)
  (define (iter lst n)
    (if (empty? lst) 0 
        (if (empty? (cdr lst)) n
            (if (= (car lst) (cadr lst))
                (iter (cdr lst) (+ n 1) ) n
                )
            )
        )
    )
  (iter (sort (tree->list tree) >) 1)

  )


(define (f4 tree)
  (if (empty? tree) 0
      (if (leaf? tree) 1
          (+ (f4 (right tree)) (f4 (left tree)))
          )
      )
  )


(define (f5 tree)
  (define (it tree)
    (if (or (empty? tree) (leaf? tree)) #t
        (if (or (empty? (left tree)) (empty? (right tree))) #f
            (and (it (left tree)) (it (right tree)))
            )
        )
    ) (if (or (empty? tree)(empty? (left tree)) (empty? (right tree))) #f (it tree)))