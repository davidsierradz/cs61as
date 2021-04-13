#lang racket
(require berkeley)

(provide (all-defined-out))

; Exercise 1
; SICP 2.7 - Define upper-bound and lower-bound
(define (add-interval x y)
 (make-interval (+ (lower-bound x) (lower-bound y))
                (+ (upper-bound x) (upper-bound y))))

(define (lower-bound interval)
 (error "Not yet implemented"))

(define (make-interval a b)
 (cons a b))

(define (mul-interval x y)
 (let ((p1 (* (lower-bound x) (lower-bound y)))
       (p2 (* (lower-bound x) (upper-bound y)))
       (p3 (* (upper-bound x) (lower-bound y)))
       (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (upper-bound interval)
 (error "Not yet implemented"))

; SICP 2.8 - Define sub-interval
(define (sub-interval x y)
 (error "Not yet implemented"))

; SICP 2.10 - Modify div-interval
(define (div-interval x y)
 (mul-interval x (make-interval (/ 1 (upper-bound y)) (/ 1 (lower-bound y)))))

; SICP 2.12 - Define make-center-percent and percent
(define (center i)
 (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-center-percent c tol)
 (error "Not yet implemented"))

(define (make-center-width c w)
 (make-interval (- c w) (+ c w)))

(define (width i)
 (/ (- (upper-bound i) (lower-bound i)) 2))

; SICP 2.17 - Define last-pair
(define (last-pair lst)
 (error "Not yet implemented"))

; SICP 2.20 - Define same-parity
(define (same-parity your-args-here)
 (error
  "Not yet implemented. Do not forget to edit the arguments of this procedure as well."))

; SICP 2.22 - Write your explanation in the comment block:
#|
Your explanation here
|#

; Exercise 2 - Define my-substitute
(define (substitute lst old new)
 (error "Not yet implemented"))

; Exercise 3 - Define my-substitute2
(define (substitute2 lst old new)
 (error "Not yet implemented"))

; Exercise 4
(define (cxr-function wd)
 ; ((cxr-function 'cddaar) (list (list (list (list 1) (list 2 3) 4) 5) 6))
 (foldr compose identity (map eval (generate-cxr-list-from-cxr-word wd))))

(define (generate-cxr-list-from-cxr-word wd)
 ;; (generate-cxr-list-from-cxr-word (word 'cdddadaadar))
 (foldl (lambda (cur acc)
         (cond
          ((equal? cur 'a)
           (append acc '(car)))
          ((equal? cur 'd)
           (append acc '(cdr)))
          (else
           acc)))
        '()
        (word->list wd)))

(define (word->list wd)
 ; (word->list (word 'cdddadaadar))
 (if (empty? wd)
  '()
  (append (list (first wd)) (word->list (butfirst wd)))))

; (cdr (cdr (car (car (list (list (list (list 1) (list 2 3) 4) 5) 6)))))
; (cddaar (list (list (list (list 1) (list 2 3) 4) 5) 6))
; (cadaar (list (list (list 1 2 3) 4 5) 6 7))
; ((foldr compose identity (list car cdr car car))
;  (list (list (list 1 2 3) 2 3) 2 3))
; (append '(car cdr car) '(cdr))
; (cadr (list 1 2 3))
; (car (cdr (list 1 2 3)))
; (cadaar (list (list (list 1 2 3) 2 3) 2 3))
; (car (cdr (car (car (list (list (list 1 2 3) 2 3) 2 3)))))
; Exercise 5
; SICP 2.18 - Define reverse
(define (reverse lst)
 ;; (reverse (list 1 4 9 16 25))
 (foldl (lambda (a b)
         (cons a b))
        '()
        lst))
