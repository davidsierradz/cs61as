#lang racket
(require (planet dyoo/simply-scheme))

(define (count-leaves tree)
 ; (count-leaves '((1 2) 3 (4 (5 (6)))))
 (cond
  ((null? tree)
   0)
  ((pair? tree)
   (+ (count-leaves (car tree)) (count-leaves (cdr tree))))
  (else
   1)))

(define (deep-reverse d-l)
 ; (deep-reverse (list (list 1 2) (list 3 4)))
 ; (deep-reverse (list 5 4 3 2 1))
 ; (cons (cons 4 (cons 3 '())) (cons (cons 2 (cons 1 '())) '()))
 ; (list (list 4 3) (list 2 1))
 ; (define x (list (list 1 2) (list 3 4)))
 ; (list (list (car (cdr (car (cdr x)))) (car (car (cdr x)))) (list (car (cdr (car x))) (car (car x))))
 (cond
  ((null? d-l)
   '())
  ((not (pair? d-l))
   d-l)
  (else
   (append (deep-reverse (cdr d-l)) (list (deep-reverse (car d-l)))))))

(define (scale-tree tree factor)
 ; (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
 (cond
  ((null? tree)
   '())
  ((not (pair? tree))
   (* tree factor))
  (else
   (cons (scale-tree (car tree) factor) (scale-tree (cdr tree) factor)))))

;;;;;;;;; Hierarchical Structures - Capital-T Trees
(define (children node)
 (cdr node))

(define (datum node)
 (car node))

(define (make-tree datum children)
 ; (children (make-tree "USA" '("California" "Massachusetts")))
 (cons datum children))
