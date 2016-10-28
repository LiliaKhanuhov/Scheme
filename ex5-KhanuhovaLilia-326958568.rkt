; Name: Khanuhov Lilia
; ID:   326958568

; Our streams API
(defmacro stream-cons (value next-expr)
  `(cons ,value (lambda () ,next-expr)))

(define stream-car car)

(define (stream-cdr stream)
  (if (procedure? (cdr stream))
      ((cdr stream))
      (cdr stream)))

; Part 1 – Simple Streams

; A

; The function generates a stream of all odd numbers, starting at start, and up
(define (generate-odds-stream start) 
  (define (helper start) 
    (stream-cons start (helper (+ start 2))))
  (helper 1))

; B

; This function has no parameters and it generates the stream of all powers of two, starting at 2 and up
(define (generate-powers-of-2-stream) 
  (define (helper number power) 
    (stream-cons (expt number power) (helper number (+ power 1))))
  (helper 2 1))

; C

; This function has no parameters and it generates the stream of sums of harmonic series from 1 to N
(define (generate-sum-harmonic) 
  (define (helper k)
    (stream-cons (helper2 k 0) (helper (+ k 1))))
  (define (helper2 number sum)
    (if (= number 0) 
        sum
        (helper2 (- number 1) (+ sum (/ 1 number)))))
  (helper 1))

; D

; This function has no parameters and it generates a stream that gives a better approximation of ex as we go further into it
(define (generate-exp-approximation x)
  (define (helper number) 
    (stream-cons (helper2 (- number 1) 1) (helper (+ number 1)))) 
  (define (helper2 i res) 
    (if (= i 0)
        res
        (helper2 (- i 1) (+ res (/ (expt x i) (factorial i))))))
  (define (factorial n)
    (if (zero? n) 1
        (* n (factorial (- n 1)))))
  (helper 1))

; Part 2 – List to Stream

; A

; The function receives a list and returns a stream with a finite length, with the same elements and orders as the list lst
(define (list-to-stream lst)
  (if (null? lst)
      ()
      (stream-cons (car lst) (list-to-stream (cdr lst)))))

; B

; The function receives a list lst (let the length of lst be len) and returns an infinite stream, where element i of the
; stream is the [i % len] element in lst
(define (list-to-infinite-stream lst) 
  (define (helper list)
    (if (null? list)
        (stream-cons (car lst) (helper (cdr lst)))
        (stream-cons (car list) (helper (cdr list)))))
  (helper lst))

; Part 3 – Merge Streams

; The function returns a stream in which the first element is the first element of the first input stream, the second 
; element is the first element of the second input stream (if exists), and so on
(define (stream-merge x . numOfArguments) 
  (define (helper1 mylist)
    (define (helper0 listOfStreams n) 
      (if (null? listOfStreams)
          (helper0 mylist (+ n 1))
          (stream-cons (stream-ref (car listOfStreams) n) (helper0 (cdr listOfStreams) n))))
    (define (stream-ref stream number)
      (if (= number 0)
          (stream-car stream)
          (stream-ref (stream-cdr stream) (- number 1))))
    (helper0 mylist 0))
(helper1 (cons x numOfArguments)))


























