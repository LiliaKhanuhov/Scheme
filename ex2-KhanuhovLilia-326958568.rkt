; Name: Khanuhov Lilia
; ID:   326958568

; Part 1 – List Manipulation 
; A 
(define (sumListElements lst) 
  (if (null? lst) 
      0 
      (+ (car lst) (sumListElements (cdr lst)))))

; B
(define (sumListPairs lst1 lst2) 
  (if (and (null? lst1) (null? lst2)) 
      null
      (cons (+ (car lst1) (car lst2)) 
            (sumListPairs (cdr lst1) (cdr lst2)))))

; C
(define (sumListPairsAny lst1 lst2) 
  (define (helper_f list1 list2 size) 
    (cond ((= size 0) ())
          ((and (null? list1) (null? list2)) ())
          ((null? list1) (helper_f lst1 list2 size)) ; If list1 was finished before list2,the helper_f will run on the lst1 from the begining
          ((null? list2) (helper_f list1 lst2 size)) ; If list2 was finished before list1,the helper_f will run on the lst2 from the begining
          (else (cons (+ (car list1) (car list2)) 
                      (helper_f (cdr list1) (cdr list2) (- size 1)))))) 
  (let ((len1 (length lst1))    ; Here I find the length of lst1 
        (len2 (length lst2)))   ; Here I find the length of lst2 
    (helper_f lst1 lst2         ; Here the helper function receives lst1,lst2 and size of longer list
              (if (> len1 len2) ; Here I find the longer list between lst1 and lst2
                  len1 
                  len2))))

; D
(define (sumAndMult lst) 
  (define (multListElements list) ; Here I define helper function that multiplies the list’s elements if the list is the element of lst
    (if (null? lst) 1
        (* (car lst) (multListElements (cdr list)))))
  (cond ((null? lst) 0)
        ((list? (car lst)) (+ (multListElements (car lst)) (sumAndMult (cdr lst))))
        (else (+ (car lst) (sumAndMult (cdr lst))))))

; Part 2 – Tail Recursion

; A
(define (findMinMax lst) 
  (let 
      ((min                                ; Here I define the default minimum (between first and second element in lst)
        (begin 
          (if (<= (car lst) (car(cdr lst))) 
              (car lst) 
              (cadr lst)))) 
       (max                                ; Here I define the default maximum (between first and second element in lst)
        (begin 
          (if (>= (car lst) (car(cdr lst))) 
              (car lst) 
              (cadr lst))))) 
    (define (f_helper list min max) 
      (cond ((null? list) (cons min (cons max list))) ; If list was finished we will return min and max that was be found up to now
            ((<= (car list) min) (f_helper (cdr list) (car list) max)) ; If was be found number that smaller then min,so it is the new min
            ((>= (car list) max) (f_helper (cdr list) min (car list))) ; If was be found number that bigger then max,so it is the new max
            ((and (> (car list) min) (< (car list) max)) (f_helper (cdr list) min max)))) ; If the number is between current the minimum and maximum,
                                                                                          ; we will move to next number
    (f_helper lst min max)))

; B
(define (findSqrt n delta) 
  (define (f_helper guess min max) 
    (cond ((< (abs (- (sqr guess) n)) delta) guess)
          ((< (-(sqr guess) n) 0) (f_helper (/ (+ guess max) 2) guess max))
          (else f_helper (/ (+ guess min) 2) min guess))) 
  (f_helper (/ n 4) 0 (/ n 2))) 

; Part 3 – Implement a FOR-loop design pattern

; A
(define (for init next continue accum pred) 
  (if (continue init accum)
      (for (next init) next continue (pred init accum) pred)
      accum
      ))

; Part 4 – Make it easier to use

; A
(define (makeNext+ value) 
  (lambda (index) (+ index value))) 

; B
(define (makeContinue<= value) 
  (lambda (index accum) (<= index value)))

; C
(define (makePredAddi) 
  (lambda (accum index) (+ accum index)))

; D
(for 1 (makeNext+ 1) (makeContinue<= 10) 1 (makePredAddi))

; Part 5 – Functions with variable number of arguments

; A
(define (sumAll x . others) 
  (if (null? others) 
      x 
      (+ x (sumListElements others))))

; B
(define (doFonAll f x . others) 
  (If (null? others) ; If the list has only one element - we will return him
      x) 
  (define (helper f list res) 
    (if (null? list) 
        res 
        (helper f (cdr list) (f res (car list))))) 
  (helper f others x)) 

; C 
(define (makeDoFonAll f) 
  (lambda (x . others) 
    (define (helper f list res) 
      (if (null? list) 
          res 
          (helper f (cdr list) (f res (car list))))) 
    (helper f others x))) 

; D

; I. 
(define multAll (makeDoFonAll (lambda (a b) (* a b)))) 

; II. 
(define sumEvens (makeDoFonAll (lambda (a b) 
                                 (if (even? a) 
                                     (if(even? b) 
                                        (+ a b) 
                                        a) 
                                     (if (even? b) 
                                         b 
                                         0)))))












