#lang Scheme
(define (func? f)
  (and (list? f) (equal? (car f) 'define)
       (or (list? (cadr f)) (pair? (cadr f)))))
(define (name-func f)
    (caadr f))
(define (body-func f)
    (cddr f))
;задание 4

(define (recursive? function name)
  (if (or (empty? function) (not (list? function))) #f
      (if (list? (car function))
          (or (recursive? (car function) name) (recursive? (cdr function) name))
          (if (equal? (car function) name) #t
              (recursive? (cdr function) name)))))
(define (divide f lst)
    (define (iter lst Tlst Flst)
      (if (empty? lst)
          (append (list Tlst Flst))
          (if (f (car lst))
              (iter (cdr lst) (cons (car lst) Tlst) Flst)
              (iter (cdr lst) Tlst (cons (car lst) Flst)))))
    (iter lst '() '()))

(define (find-recursive function name)

  (define (divide f lst)
    (define (iter lst Tlst Flst)
      (if (empty? lst)
          (append (list Tlst Flst))
          (if (f (car lst))
              (iter (cdr lst) (cons (car lst) Tlst) Flst)
              (iter (cdr lst) Tlst (cons (car lst) Flst)))))
    (iter lst '() '()))
  
  (define divided (divide func? function))
  (append (foldl (lambda (func res)
                   (append res (find-recursive (body-func func) (name-func func))))
                 '()
                 (car divided))
          (filter-not empty? (foldl (lambda (f result)
                                      (if (recursive? f name) (cons (cons name 'recursive) result)
                                          (if name
                                              (cons (cons name 'non_recursive) result)
                                              (cons '() result))))
                                     '() (cdr divided)))))
                                          
                     

(define (z4 inFile-4 no-recursive-file recursive-file)
  (define in (open-input-file inFile-4))
  (define nonrec-out (open-output-file no-recursive-file #:exists 'replace))
  (define rec-out (open-output-file recursive-file #:exists 'replace))
  (define (iter)
    (define ch (read in))
    (if (equal? ch eof)
        (begin
          (close-input-port in)
          (close-output-port nonrec-out)
          (close-output-port rec-out))
        (begin
          (map (lambda (function)
                 (if (equal? (cdr function) 'recursive) (displayln (car function) rec-out)
                     (displayln (car function) nonrec-out)))
               (find-recursive (cons ch '()) #f))
          (iter))))
  (iter))