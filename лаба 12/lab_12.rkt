#lang Scheme
(define % remainder)
(define // quotient)
(define (f1 n s)
  
  (define (summ-digit x)
    (define (it x s)
      (if (= 0 x) s (it (// x 10) (+ s (% x 10))))
      ) (it x 0)
    )
  (define (bliz a b x)
  (if (> (abs (- x a)) (abs (- x b))) b a
      ))
  (define  razn (- (summ-digit n) s))
  (define (p_in_d x a raz)
    (if (= a 0) x (if (= (% x raz) 9)
                      (p_in_d (+ x raz) (- a 1) (* raz 10))
                      (p_in_d (+ x (/ raz 10)) (- a 1) raz))
        ))
  
  (define (des->devi x)
    (define kol_d (// x 9))
    (define (it a n raz)
      (if (= n 0) (+ a (* (- x (* kol_d 9)) raz))
          (it (+ a (* 9 raz)) (- n 1) (* 10 raz))
          )) (it 0 kol_d 1))
  (define (new_minus x n)
    (define (it x n raz)
      (if (= 0 n) x
          (if (= 0 (% x (* raz 10)))
              (it (- x (* raz 10)) (- n 1) (* raz 10))
              (it (- x raz) (- n 1) raz))))
    (it x n 1))
  (define (new_digit x s)
    (define (it x rez)
      (if (= x 0) (p_in_d rez (- s 1) 10)
          (it (// x 10) (* rez 10))
          )
      )(it x 1)
    )

  
  (define  (kol_dev_in_dig n)
    (define (it x n raz)
      (if (= n 0) x (it (+ x (* 9 raz)) (- n 1) (* raz 10))
          )) (it 0 n 1))

  
  (if (= s 0) 0
      (if (>= razn 0) (bliz (new_minus n razn) (new_digit n s) n) 
          (if (> 9 (abs razn))  (p_in_d n (abs razn) 10)
              (let ((ost (- s (* 9 (// (abs razn) 9))))
                    (kol_d (// (abs razn) 9)))
                (+ (* (des->devi ost) (expt 10 kol_d)) (kol_dev_in_dig kol_d))

                )
              ))
      ))


 (define (f2 lst)
  (define n (length lst))
  (define s_lst (sort  lst <))
  (define (it1 lst a)
    (if (empty? lst) #f
        (let ((out (it2 a lst) ))
          (if (empty? out) (it1 (cdr lst) (car lst)) out)
          )
    ))
  (define (it2 a lst )
    (if (empty? lst) null
        (let ((out (it3 a (car lst) (remove a (remove (car lst) s_lst)))))
        (if (empty? out) (it2 a (cdr lst)) out))))
    (define (it3 a b lst)
      (if (empty? lst) null
          (if (= (+ a b) (car lst)) (list a b (+ a b)) (it3 a b (cdr lst))
      )))
  (if (> n 2)  (it1 (cdr s_lst) (car s_lst)) #f))
  




(define (f3 a b)
  (define (it a b)
    (if (= (% (max a b) (min a b)) 0)
        (if (= 1 (/ a b)) a
            (it (/ (max a b) (min a b)) (min a b)))
        #f
        ))
  (if (= a b) a
  (if (> 2 (min a b)) #f (it a b)))
  )


(define (f4 str)
  (define (it lst pred sum)
    (if (empty? lst) (+ sum pred)
        (let ((c (-(char->integer (car lst)) 48)))
          (if (and (>= c 0) (< c 10))
              (it (cdr lst) (+ (* pred 10) c) sum)
              (it (cdr lst) 0 (+ sum pred))
              )
          )
        )
    ) (it (string->list str) 0 0)
  )

(define (f5 x)
  (define (prime? x) 
    (define (it d)
      (if (and (>= x (* d d)) (not (= (% x d) 0 )))
          (it (+ d 1))
          (> (* d d) x)
          ))
    (if (= 0 (% x 2)) (= x 2)
        (it 3)
        )
    )
  (define (num->lst x)
    (define (iter x lst)
      (if (= x 0) lst
          (iter (// x 10) (cons (% x 10) lst))
          )
      )
    (iter x null)
    )
  
  (define (lst->num lst)
    (define (iter lst n)
      (if (empty? lst) n
          (iter (cdr lst) (+ (* 10 n) (car lst)))
          )
      )
    (iter (cdr lst) (car lst))
    )
  (define (dob lst x n)
    (if (= n 0) (cons x lst) (cons (car lst) (dob (cdr lst) x (- n 1)))))
  
  (define x_lst (num->lst x))
  (define x_length (length x_lst))
  (define (it x n lst)
    (if (= n -1) (if (empty? lst) #f (apply max lst))
        (let ((new_xs (it2 x_lst n x_length null)))
          (it x (- n 1) (if (empty? new_xs) lst (append new_xs lst)))
          )))
  (define (it2 x_lst n x_l lst)
    (if (> 0 x_l) lst
        (let ((new_x (lst->num (dob x_lst n (- x_length x_l)))))
          (it2 x_lst n (- x_l 1) (if (prime? new_x) (cons new_x lst) lst))
          )))
  
  (it x 9 null)
  )












