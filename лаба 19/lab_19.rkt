#lang Scheme

(define (f1 f_in f_out . params)
  (define in (open-input-file  f_in))
  (define out (open-output-file f_out #:exists 'replace))
  (define vec (apply vector params))

  (define (it state n)
    (define ch (read-char in))
    (if (equal? ch eof) (begin
                          (close-input-port in)
                          (close-output-port out))
        (if (or (equal? ch #\return) (equal? ch #\newline))
            (begin (display ch out) (it state n))
            (cond
              ((= state 0) (if (equal? ch #\%)
                               (it 1 n)
                               (begin (display ch out) (it 0 n))))
              ((= state 1) (if (equal? ch #\%)
                               (begin (display ch out) (it 0 n))
                               (it 2 (+ (* n 10)(- (char->integer ch) 48)))))
              ((= state 2) (if (equal? ch #\%)
                               (begin
                                 (display (vector-ref vec (- n 1)) out)
                                 (it 0 0))
                               (it 2 (+ (* n 10)(- (char->integer ch) 48))
                                   )))))))
  (it 0 0))

(define (f2)
  (define in (open-input-file  "in.txt"))
  (define out (open-output-file "out.txt" #:exists 'replace))
  
  (define (num->lst n lst)
    (if (= 0 n) lst
        (num->lst (quotient n 10) (cons (remainder n 10) lst))))
  (define (write_rez lst)
    (if (empty? lst) (close-output-port out)
        (begin (writeln (car lst) out) (write_rez (cdr lst)))))
        

                
  (define (it state n1 n2 lst)
    (define ch (read-char in))
    (if (equal? ch eof) (begin
                          (close-input-port in)
                          (write_rez (sort (if (= n2 0) lst
                                               (let ((o (num->lst (+ n1 n2) null)))
                                                 (if (equal? o (sort o <))
                                                     (cons (+ n1 n2) lst) lst))) >)))
                         
        (cond
          ((= state 0) 
           (if (equal? ch #\+) (it 1 n1 n2 lst)
               (it 0
                   (+ (* n1 10) (- (char->integer ch) 48))
                   n2 lst)))
          ((= state 1)
           (if (equal? ch #\return)
               (begin (read-char in)
                      (let ((o (num->lst (+ n1 n2) null)))
                        (it 0 0 0 (if (equal? o (sort o <))
                                      (cons (+ n1 n2) lst) lst))))
               (it 1 n1 (+ (* n2 10) (- (char->integer ch) 48)) lst))))))
  (it 0 0 0 null))

(define (f3 f_in)
  (define in (open-input-file  f_in)) 
  (define (it state n)
    (define new_char (read-char in))
    (cond
      ((equal? new_char eof) (begin (close-input-port in) n))
      ((or (equal? new_char #\( ) (equal? new_char #\[ ) (equal? new_char #\{ )) 
       (cond
         [(= state 0) (it 1 n)]
         [(= state 7) (it 0 (+ n 1))]
         [(= state 13) (it 0 n)]
         [(and (>= state 8) (<= state 16)) (it state n)]
         [else (it 0 n)]
         )
       )
      ((and (equal? new_char #\d) (= state 1) ) (it 2 n))
      ((equal? new_char #\e)
       (cond
         [(and (>= state 8) (<= state 16)) (it state n)]
         [(= state 2) (it 3 n)]
         [(= state 6) (it 7 n)]
         [else (it 0 n)]
         )
       )
      ((and (equal? new_char #\f) (= state 3)) (it 4 n))
      ((and (equal? new_char #\i) (= state 4)) (it 5 n))
      ((and (equal? new_char #\n) (= state 5)) (it 6 n))

      ((or (equal? new_char #\newline) (equal? new_char #\return))
       (it (if (or (= state 8) (and (not (= state 11)) (>= state 0) (<= state 15)))
               0 state) n))
      ((equal? new_char #\space)
       (it (if (or (= state 1)
                     (= state 7)
                     (= state 8)
                     (= state 11)
                     (= state 15)) state 0) n))
      ((equal? new_char #\;) (it (if (>= state 8) state 8) n))
      ((equal? new_char #\#)
       (cond
         [(= state 8) (it 8 n)] 
         [(= state 9) (it 10 n)] 
         [(= state 10) (it 10 n)]
         [(= state 11) (it 11 n)]
         [(= state 12) (it 0 n)]
         [(= state 13) (it 13 n)]
         [else (it 9 n)]
         )
       )
      ((equal? new_char #\|)
       (cond
         [(= state 9) (it 11 n)]
         [(= state 11) (it 12 n)]
         [else (it state n)]
         )
       )
      ((equal? new_char #\\)
       (cond
         [(= state 8) (it 8 n)]
         [(= state 11) (it 11 n)]
         [(= state 9) (it 13 n)]
         [(= state 13) (it 0 n)]
         [(= state 15) (it 16 n)]
         [(= state 16) (it 15 n)]
         [else (it 14 n)]
         )
       )
      ((equal? new_char #\")
       (cond
         [(= state 8) (it 8 n)]
         [(= state 11) (it 11 n)]
         [(= state 15) (it 0 n)]
         [(= state 16) (it 15 n)]
         [else (it 15 n)]
         )
       )
      (else (it (if (or (= state 8) (= state 11)) state 0) n))
      )
    )
  (it 0 0)
  )
              
    
  




