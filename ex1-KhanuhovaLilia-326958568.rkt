; Name: Khanuhov Lilia
; ID:   326958568

; Part 1

; A    
; I.   ((3 + (4 - 2)) - ((10 / 2) * ((3 + 4) * 3)))
; II.  (5 - (3 + (4 - ((1 + 2) + (4 - 3)))))
; III. (((((((7 * 8) * 6) * 5) * 4) * 3) * 2) * 1)

; B
; I.   (+ (/ (+ (- 2 1) 4) (* (+ 3 2) (+ 5 8))) (- 3 1)) 
; II.  (* (+ (- 4 5) 3) (* (/ (+ 3 2) 2) (+ 4 (- 3 2))))
; III. (+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 7))))))

; Part 2

; A     
(define (between x a b)
  (cond ((and (<= x b) (>= x a)) 'yes)
        (else 'no)))
                                    
; B
(define (circle-area r)
  (let ((pi 3.14)) (* pi r r)))

; Part 3
(define (pow x n)
  (if (= n o) 
      1
      (* x (pow x (- n 1)))))

; Part 4
; The function displays instructions on the notes/coins to be used
; in order to pay the given amount in the fewest coins possible.
(define (how-to-pay amount) 
  (define (f_helper amount) 
    (cond ((>= amount 200) (begin 
                             (let ((q200 (quotient amount 200))) 
                               (begin 
                                 (display q200) 
                                 (display " coins/notes of 200")) 
                               (if (> (modulo amount 200) 0) (newline)) 
                               (f_helper (modulo amount 200))))) 
          ((>= amount 100) (begin 
                             (display "1 coins/notes of 100") 
                             (if (> (modulo amount 100) 0) (newline)) 
                             (f_helper (- amount 100))))
          ((>= amount 50) (begin 
                            (display "1 coins/notes of 50") 
                            (if (> (modulo amount 50) 0) (newline)) 
                            (f_helper (- amount 50))))
          ((>= amount 20) (begin 
                            (let ((q20 (quotient amount 20))) 
                              (begin 
                                (display q20) 
                                (display " coins/notes of 20")) 
                              (if (> (modulo amount 20) 0) (newline)) 
                              (f_helper (modulo amount 20))))) 
          ((>= amount 10) (begin 
                            (display "1 coins/notes of 10") 
                            (if (> (modulo amount 10) 0) (newline)) 
                            (f_helper (- amount 10))))
          ((>= amount 5) (begin 
                           (display "1 coins/notes of 5") 
                           (if (> (modulo amount 5) 0) (newline)) 
                           (f_helper (- amount 5))))
          ((>= amount 2) (begin 
                           (let ((q2 (quotient amount 2))) 
                             (begin 
                               (display q2) 
                               (display " coins/notes of 2")) 
                             (if (> (modulo amount 2) 0) (newline)) 
                             (f_helper (modulo amount 2))))) 
          ((>= amount 1) (begin 
                           (display "1 coins/notes of 1") 
                           (if (> (modulo amount 1) 0) (newline)) 
                           (f_helper (- amount 1)))))) 
  (f_helper amount))




  
