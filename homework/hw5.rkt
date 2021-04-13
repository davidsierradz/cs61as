#lang racket
(require berkeley)

(provide (all-defined-out))

; Exercise 1{{{
; What are the result of the expressions? Make sure to comment your answer out.
(define x (list 1 2 3))
(define y (list 4 5 6))

; (list 1 2 3 4 5 6)
; (append x y)
; weird
; (cons (list 1 2 3) (list 4 5 6))
; (cdr (cons (list 1 2 3) (list 4 5 6)))
; (cons (cons 1 (cons 2 (cons 3 '()))) (cons 4 (cons 5 (cons 6 '()))))
; (cons x y)
; '((1 2 3) (4 5 6))
; (list x y)
#| }}} |#

; Exercise 2 Mobile{{{
; (define (make-branch length structure)
;  ; (make-branch 1 4)
;  ; (make-branch 2 (make-mobile (make-branch 1 1) (make-branch 2 2)))
;  (list length structure))
;
; (define (make-mobile left right)
;  ; (make-mobile (make-branch 2 (make-mobile (make-branch 1 1) (make-branch 2 2))) (make-branch 3 3))
;  (list left right))
; a. Define left-branch, right-branch, branch-length, and
; branch-structure.
; (define (branch-length branch)
;  (first branch))
;
; (define (branch-structure branch)
;  (last branch))
;
; (define (left-branch mobile)
;  (first mobile))
;
; (define (right-branch mobile)
;  (last mobile))
; b. Define total-weight.
(define (total-weight mobile)
 ; (total-weight (make-mobile (make-branch 2 (make-mobile (make-branch 1 1) (make-branch 2 2))) (make-branch 3 3)))
 (cond
  ((number? mobile)
   mobile)
  (else
   (+ (total-weight (branch-structure (left-branch mobile)))
      (total-weight (branch-structure (right-branch mobile)))))))

; c. Define balanced?
(define (balanced? b-mobile)
 ; (balanced? (make-mobile (make-branch 3 6) (make-branch 9 2)))
 ; (balanced? (make-mobile (make-branch 2 5) (make-branch 3 3)))
 ; (balanced? (make-mobile (make-branch 3 (make-mobile (make-branch 4 6) (make-branch 3 8))) (make-branch 6 7)))
 (cond
  ((number? b-mobile)
   #t)
  (else
   (and (= (* (branch-length (left-branch b-mobile))
              (branch-weight (left-branch b-mobile)))
           (* (branch-length (right-branch b-mobile))
              (branch-weight (right-branch b-mobile))))
        (balanced? (branch-structure (left-branch b-mobile)))
        (balanced? (branch-structure (right-branch b-mobile)))))))

(define (branch-weight branch)
 ; (branch-weight (make-branch 2 3))
 ; (branch-weight (make-branch 2 (make-mobile (make-branch 2 (make-mobile (make-branch 2 3) (make-branch 3 2))) (make-branch 3 2))))
 (cond
  ((number? (branch-structure branch))
   (branch-structure branch))
  (else
   (+ (branch-weight (left-branch (branch-structure branch)))
      (branch-weight (right-branch (branch-structure branch)))))))

; d. Redefine all the necessary procedures to work with the new
; constructors given below.
; Make sure that only one set of constructors is active at any time
; (otherwise Racket will complain about duplicate defintions).
(define (branch-length branch)
 (car branch))

(define (branch-structure branch)
 (cdr branch))

(define (left-branch mobile)
 (car mobile))

(define (make-branch length structure)
 (cons length structure))

(define (make-mobile left right)
 (cons left right))

(define (right-branch mobile)
 (cdr mobile))

#| }}} |#

; Exercise 3a - Define square-tree{{{
(define (square-tree d-l)
 ; (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
 (cond
  ((number? d-l)
   (square d-l))
  (else
   (map square-tree d-l))))

; Exercise 3b - Define tree-map
(define (tree-map fn tree)
 ; (define (square-tree tree) (tree-map square tree))
 ; (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
 (map (lambda (element)
       (if (not (list? element))
        (fn element)
        (tree-map fn element)))
      tree))

#| }}} |#

; Exercise 4 -  Complete the definition of accumulate-n{{{
(define (accumulate-n op init seqs)
 ; (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
 ; (map (lambda (x) (cdr x)) '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
 ; (foldl + 0 (foldr (lambda (cur acc) (cons (first cur) acc)) '() '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
 (if (null? (car seqs))
  '()
  (cons (foldr op
               init
               (foldr (lambda (cur acc)
                       (cons (first cur) acc))
                      '()
                      seqs))
        (accumulate-n op
                      init
                      (map (lambda (x)
                            (cdr x))
                           seqs)))))

#| }}} |#

; Exercise 5 - Complete the definitions of matrix-*-vector, transpose,{{{
; and matrix-*-matrix.
(define (dot-product v w)
 (foldr + 0 (map * v w)))

(define (matrix-*-matrix m n)
 (let ((cols (transpose n)))
  (map "YOUR CODE HERE" m)))

(define (matrix-*-vector m v)
 (map "YOUR CODE HERE" m))

(define (transpose mat)
 (accumulate-n "YOUR CODE HERE" "YOUR CODE HERE" mat))

#| }}} |#

; Exercise 6 - Give the property that op should satisfy:{{{
#|

Your property here

|#
#| }}} |#

; Exercise 7 - Define equal?{{{
(define (my-equal? l1 l2)
 (error "Not yet implemented"))

#| }}} |#

; Exercise 8 - Complete the definition of subsets{{{
(define (subsets s)
 (if (null? s)
  (list '())
  (let ((rest (subsets (cdr s))))
   (append rest (map "YOUR CODE HERE" rest)))))

#| }}} |#

; Exercuse 9 - Modify the calc program{{{
;; Racket calculator -- evaluate simple expressions
; The read-eval-print loop:
(define (calc)
 (display "calc: ")
 (flush-output)
 (print (calc-eval (read)))
 (calc))

; Evaluate an expression:
(define (calc-eval exp)
 (cond
  ((number? exp)
   exp)
  ((list? exp)
   (calc-apply (car exp) (map calc-eval (cdr exp))))
  (else
   (error "Calc: bad expression:" exp))))

; Apply a function to arguments:
(define (calc-apply fn args)
 (cond
  ((eq? fn '+)
   (foldr + 0 args))
  ((eq? fn '-)
   (cond
    ((null? args)
     (error "Calc: no args to -"))
    ((= (length args) 1)
     (- (car args)))
    (else
     (- (car args) (foldr + 0 (cdr args))))))
  ((eq? fn '*)
   (foldr * 1 args))
  ((eq? fn '/)
   (cond
    ((null? args)
     (error "Calc: no args to /"))
    ((= (length args) 1)
     (/ (car args)))
    (else
     (/ (car args) (foldr * 1 (cdr args))))))
  (else
   (error "Calc: bad operator:" fn))))

#| }}} |#
