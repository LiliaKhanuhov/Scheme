; Name: Khanuhov Lilia
; ID:   326958568

; Part 1: Basic Tree API

; A 
; Returns a new node with the given details
(define (makeNode value left right color) 
  (list value left right color))

; B
; Returns left child (a node) of the given node
(define (getLeft node) 
  (cadr node))

; C
; Returns right child (a node) of the given node
(define (getRight node) 
  (caddr node))

; D
; Returns the value of the given node
(define (getValue node) 
  (car node))

; E
;Returns the color of the given node
(define (getColor node) 
  (cadddr node))

; F
; Returns a new node that represents a leaf with the given value
(define (makeLeaf value) 
  (list value () () 'red))

; Part 2: Tree Insert
; The function inserts a new node whose value is given (as a red leaf), into the given sorted tree
(define (insert tree value)
  (cond ((null? tree) (makeLeaf value))
        ((< value (getValue tree)) (makeNode (getValue tree)
                                             (insert (getLeft tree) value)
                                             (getRight tree) 'red))
        (else (makeNode (getValue tree)
                        (getLeft tree)
                        (insert (getRight tree) value) (getColor tree)))))

; Part 3: Rotation

; A
; The function returns the given tree after the sub-tree whose root value is v was rotated to the left
(define (leftRotate tree v)
  (cond ((eq? v (getValue tree)) (makeNode (getValue (getRight tree)) 
                                           (makeNode v (getLeft tree) (getLeft (getRight tree)) (getColor tree)) 
                                           (getRight (getRight tree)) (getColor (getRight tree))))
        ((< v (getValue tree)) (makeNode (getValue tree) (leftRotate (getLeft tree) v) (getRight tree) (getColor tree)))
        (else (makeNode (getValue tree) (getLeft tree) (leftRotate (getRight tree) v) (getColor tree)))))

; B
; The function returns the given tree after the sub-tree whose root value is v was rotated to the right
(define (rightRotate tree v)
  (cond ((eq? v (getValue tree)) (makeNode (getValue (getLeft tree)) 
                                           (getLeft (getLeft tree)) (makeNode v (getRight (getLeft tree)) (getRight tree) 
                                                                              (getColor tree)) (getColor (getLeft tree))))
        (( < v (getValue tree)) (makeNode (getValue tree) (rightRotate (getLeft tree) v) (getRight tree) (getColor tree)))
        (else (makeNode (getValue tree) (getLeft tree) (rightRotate (getRight tree) v) (getColor tree)))))

; Part 4: Recoloring
; The function returns the given tree after the color of the node whose value is v was changed to color
(define (recolor tree v color) 
  (cond ((eq? v (getValue tree)) (makeNode (getValue tree) (getLeft tree) (getRight tree) color))
        ((< v (getValue tree)) (makeNode  (getValue tree) (recolor (getLeft tree) v color) (getRight tree) (getColor tree)))
        (else (makeNode (getValue tree) (getLeft tree) (recolor (getRight tree) v color) (getColor tree)))))

; Part 5: Fixing the tree after insertion

; This function receives a whole tree (the result of the insert operation) and the value that was just added v.
(define (rbInsertFixup tree v)
  ; This function is searching the node to be fixed and should always know the parent and grandparent of the node it examines
  (define (search searchTree v parent grandparent)
    (cond ((< v (getValue searchTree)) (search (getLeft searchTree) v searchTree parent)) 
          ((> v (getValue searchTree)) (search (getRight searchTree) v searchTree parent))
          (else (fixeUp searchTree v parent grandparent)))) 
  ; This function fixs the tree so that it would become a valid red-black tree. The function returns a new tree
  ; and does not actually change the input tree
  (define (fixeUp nodeV v parent grandparent)
    (cond ((null? parent) (recolor nodeV v 'black))
          ((null? grandparent) tree)
          ((and (eq? (getColor parent) 'red) (eq? (getColor nodeV) 'red))  
           (cond 
             ; case 1
             ((and (not (null? (getLeft grandparent))) (eq? (getValue (getLeft grandparent)) (getValue parent)) (eq? (getColor (getRight grandparent)) 'red)) 
              (rbInsertFixup (reColor (recolor (reColor tree (getValue parent) 'black) (getValue (getRight grandparent)) 'black) 
                                      (getValue grandparent) 'red) (getValue grandparent)))
             ((and (not (null? (getRight grandparent))) (eq? (getValue (getRight grandparent)) (getValue parent)) (not (null? (getLeft grandparent))) 
                   (eq? (getColor (getLeft grandparent)) 'red)) (rbInsertFixup (reColor (recolor (reColor tree (getValue parent) 'black) 
                                                                                                 (getValue (getLeft grandparent)) 'black) 
                                                                                        (getValue grandparent) 'red) (getValue grandparent)))
             ; case 2.A
             ((and (not (null? (getLeft grandparent))) (eq? (getValue (getLeft grandparent)) (getValue parent)) 
                   (or (eq? (getColor (getRight grandparent)) 'black) (null? (getRight grandparent))) 
                   (eq? (getValue (getleft parent)) (getValue nodeV))) (rightRotate (reColor (reColor tree (getValue parent) 'black) 
                                                                                             (getValue grandparent) 'red) (getValue grandparent)))
             ; case 2.B
             ((and (eq? (getValue (getRight grandparent)) (getValue parent)) (or (null? (getLeft grandparent)) (eq? (getColor (getLeft grandparent)) 'black)) 
                   (eq? (getValue (getRight parent)) (getValue nodeV))) (leftRotate (reColor (reColor tree (getValue parent) 'black) (getValue grandparent) 'red) 
                                                                                    (getValue grandparent))) 
             ; case 3.A
             ((and (eq? (getValue (getLeft grandparent)) (getValue parent)) (or (eq? (getColor (getRight grandparent)) 'black) (null? (getRight grandparent)))
                   (eq? (getValue (getRight parent)) (getValue nodeV)))  (rbInsertFixup (leftRotate tree (getValue parent)) (getValue parent)))
             ; case 3.B
             ((and (eq? (getValue (getRight grandparent)) (getValue parent)) (or (eq? (getColor (getLeft grandparent)) 'black) (null? (getLeft grandparent)))
                   (eq? (getValue (getLeft parent)) (getValue nodeV)))  (rbInsertFixup (rightRotate tree (getValue parent)) (getValue parent)))
             ))
          (else tree)))
  (search tree v () ()))

; Part 6: Wrap the insert operation
; The function returns a valid red-black tree which is the result of inserting a new node whose value is "value" into the red-black tree "tree"
(define (rbInsert tree value)
  (rbInsertFixup (insert tree value) value)) 











