#lang racket
(require (planet dyoo/simply-scheme))

(define (func-pair pair)
 ; (func-pair (cons (lambda (x) (* x x)) 10))
 ((car pair)
  (cdr pair)))

(car (cons 1 (cons 2 '())))
(cdr (cons 2 '()))
(list-ref '(1
            2
            3)
          0)
(sentence 1 2 3)

;; functions:
;; car
;; cdr
;; cadr...
;; cons
;; list
;; append
;; list-ref
;; null?
;; length
;; foldl
;; foldr
(define (end-segment segment)
 (cdr segment))

(define (length-segment segment)
 ; (length-segment (make-segment (make-point -2 8) (make-point -7 -5)))
 (sqrt
  (+
   (sqr
    (abs (- (x-coord (start-segment segment)) (x-coord (end-segment segment)))))
   (sqr
    (abs (- (y-coord (start-segment segment)) (y-coord (end-segment segment))))))))

(define (make-point x y)
 (cons x y))

(define (make-segment a b)
 (cons a b))

(define (start-segment segment)
 (car segment))

(define (x-coord point)
 (car point))

(define (y-coord point)
 (cdr point))
