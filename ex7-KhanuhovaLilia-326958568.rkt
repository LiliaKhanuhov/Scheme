; Name: Khanuhov Lilia
; ID:   326958568

(define board-easy '((? ? 2 ? 1 ? 8 ? ?)
                     (? 4 ? 9 3 ? 5 ? 2)
                     (? 8 5 ? ? 7 ? 4 ?)
                     (? 9 ? ? ? 1 4 ? ?)
                     (? 2 1 7 ? ? 3 5 ?)
                     (? ? 8 ? 4 5 ? 9 ?)
                     (? 6 ? 1 ? ? 2 3 ?)
                     (2 ? 9 ? 5 3 ? 6 ?)
                     (? ? 3 ? 6 ? 1 ? ?)))

(define board-hard '((? 8 2 ? 1 ? ? ? ?)
                     (? ? 7 2 ? ? ? 3 4)
                     (? ? ? ? ? 7 ? ? ?)
                     (9 ? ? ? 5 ? 7 ? ?)
                     (? 6 ? 1 ? ? 3 ? ?)
                     (? ? ? 4 ? 2 ? ? ?)
                     (? ? ? 6 ? ? 1 2 ?)
                     (8 ? ? ? ? ? ? 6 ?)
                     (? 3 ? 5 ? 9 ? ? 8)))

; Part 1: Board API

; A.
; The function receives a Sudoku board and two indices for row and column (each index is
; an integer from 0 to 8, inclusive). The function returns the value in the requested cell
(define (get-cell board row col) 
  (list-ref (list-ref board row) col))

; B.
; The function receives a Sudoku board, two indices for row and column (each index is an
; integer from 0 to 8, inclusive), and a value. The function returns a new board in which 
; the value in the requested cell is value.
(define (set-cell board row col value) 
  (define (helper myBoard n) 
    (cond ((= n 8) (list (changeRow (car myBoard) 0)))
          ((= n row) (cons (changeRow (car myBoard) 0) (cdr myBoard)))
          (else (cons (car myBoard) (helper (cdr myBoard) (+ n 1))))))
  (define (changeRow myRow n)
    (cond ((= n 8) (list value))
          ((= n col) (cons value (cdr myRow)))
          (else (cons (car myRow) (changeRow (cdr myRow) (+ n 1))))))
  (helper board 0))

;(define (set-cell board row col value) 
;  (define (helper myBoard n) 
;    (cond ((= n 8) (list (changeRow (list-ref myBoard n) 0)))
;          ((= n row) (cons (changeRow (car myBoard) 0) (cdr myBoard)))
;          (else (cons (car myBoard) (helper (cdr myBoard) (+ n 1))))))
;  (define (changeRow myRow n)
;    (cond ((= n 8) (list value))
;          ((= n col) (cons value (cdr myRow)))
;          (else (cons (car myRow) (changeRow (cdr myRow) (+ n 1))))))
;  (helper board 0))

; C.
; The function receives a Sudoku board and a row index (integer in [0,8]). It returns the 
; requested row as a single-dimensional list.
(define (get-row board row)
  (list-ref board row))

; D.
; The function receives a Sudoku board and a column index (integer in [0,8]). It returns 
; the requested column as a single-dimensional list (as a row).
(define (get-col board col) 
  (define (helper n)
    (if (= n 8)
        (list (list-ref (get-row board n) col))
        (cons (list-ref (get-row board n) col) (helper (+ n 1)))))
  (helper 0))

; E.
; The function receives a Sudoku board and the row and column of the requested 3x3 square.
; The function returns the values in the requested square, as a single-dimension list, 
; one row's value after each other.
(define (get-square board sq-row sq-col)
  (define (helper row col limit)
    (append (inner row col limit) (inner (+ row 1) col limit) (inner (+ row 2) col limit)))
  (define (inner r c limit) 
    (if (< c limit)
        (cons (get-cell board r c) (inner r (+ c 1) limit))
        (list (get-cell board r c))))
  (cond ((= sq-row 0) 
         (cond ((= sq-col 0) (helper 0 0 2))
               ((= sq-col 1) (helper 0 3 5))
               ((= sq-col 2) (helper 0 6 8))))
        ((= sq-row 1)
         (cond ((= sq-col 0) (helper 3 0 2))
               ((= sq-col 1) (helper 3 3 5))
               ((= sq-col 2) (helper 3 6 8))))
        ((= sq-row 2)
         (cond ((= sq-col 0) (helper 6 0 2))
               ((= sq-col 1) (helper 6 3 5))
               ((= sq-col 2) (helper 6 6 8))))))

; Part 2: Checking Validity of a Zone
(define (is-valid-zone zone) 
  (define (helper list) 
    (define (helper_2 lst number)
      (cond ((null? lst) (helper (cdr list)))
            ((eq? (car lst) number) #f) 
            (else (helper_2 (cdr lst) number))))
    (cond ((null? list) #t)
          ((symbol? (car list)) (helper (cdr list)))
          ((number? (car list)) (helper_2 (cdr list) (car list))))) 
  (helper zone))

; Part 3: Guessing the Value of a Cell
; The function receives a board, row and column indices, and a guess value.It either
; returns a new board, in which cell (row, col) is value, or it returns #f – when 
; guess value is illegal for this cell.
(define (guess-cell-value board row col value) 
  (cond ((eq? (get-cell board row col) '?) 
         (let ((possible_board (set-cell board row col value)))
           (if (and (is-valid-zone (get-row possible_board row)) (is-valid-zone (get-col possible_board col)) 
                    (is-valid-zone (get-square possible_board 
                                               (cond ((or (eq? row '0) (eq? row '1) (eq? row '2)) '0)
                                                     ((or (eq? row '3) (eq? row '4) (eq? row '5)) '1)
                                                     (else '2)) 
                                               (cond ((or (eq? col '0) (eq? col '1) (eq? col '2)) '0)
                                                     ((or (eq? col '3) (eq? col '4) (eq? col '5)) '1)
                                                     (else '2)))))
               possible_board
               #f)))
        ((eq? (get-cell board row col) value) board)
        (else #f)))

; Part 4: Solve the Game!
; This is the main solver function, which uses backtracking to solve the game. 
; It either returns a solved board (with no ? symbols and where each zone is legal),
; or #f if there is no solution for the given game.
; The main idea of the algorithm should be: go over the nine possible digits, one by
; one, and try to place it in each zone.
(define (solve-game board) 
  (define (sub-game board row col n) 
    (cond ((> n 9) board) ; The board is full
          ((> row 8) (sub-game board 0 0 (+ n 1))) ; I finished to fill out the some 
                                                   ; digit in the board
          ((> col 8) #f) ; no solution
          ; if a row already contains the digit, it should be skipped
          ((member n (get-row board row)) (sub-game board (+ row 1) col n))
          (else
           (let ((try (guess-cell-value board row col n)))
             (if (not try)
                 (sub-game board row (+ col 1) n)
                 (let ((try_again (sub-game try (+ row 1) 0 n)))
                   (if try_again 
                       try_again
                       (sub-game board row (+ col 1) n))))))))
          (sub-game board 0 0 1))
    
    
  
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    