; Name: Khanuhov Lilia
; ID:   326958568

; Part 1 – Simple macro

; A.
(defmacro absolute (x) 
  `(let ((_x ,x)) 
     (if (< _x 0) 
         (* _x -1)
         _x)))

; B.
(defmacro cone-volume (radius height) 
  `(let ((r ,radius) (h ,height)) 
     (* (/ 1 3) pi (* r r) h)))

; Part 2 – A bit harder macro
(defmacro multListBy2 (lst) 
  `(begin 
     (define (helper list) 
       (if (null? list) 
           () 
           (cons (* 2 (car list)) (helper (cdr list)))))
     (helper ,lst)))

; Part 3 – Less-Than macro

; A
(defmacro less-than1 (x . rules)  
  (define (helper list) 
    `((< result ,(car list)) ,(cadr list))) ; Here I compare the value of x to the values in the rules, according to arguments order 
                                            ; If x is less than a value of some rule, then the expression of this rule is evaluated 
  `(let ((result ,x))
     (cond ,@(map helper rules))))

; B
(defmacro less-than2 (x . rules) 
  (define (helper list) 
    `((< result ,(car list)) ,(cadr list))    ; Here I compare the value of x to the values in the rules, according to arguments order
    `((eq? 'else' ,(car list)) ,(cadr list))) ; If no rule matches the value of x, then the expression of the else clause should be evaluated  
  `(let ((result ,x))
     (cond ,@(map helper rules))))

; Part 4 – Boolean Logic

; This code contains the implementation of an OR macro
(defmacro nor (x . arguments) 
  (define (iter-or lst) ; This is implementation of OR macro
    (if (null? lst) 
        `val 
        `(let ((val ,(car lst)))
           (if  val
                #t
                ,(iter-or (cdr lst))))))           
  `(not ,(iter-or (cons x arguments)))) ; Here I make OR macro to NOR macro


