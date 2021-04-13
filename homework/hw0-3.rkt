#lang racket
(require berkeley)

(provide (all-defined-out))

; [Simply Scheme: Introducing Computer Science ch 11: Introduction to Recursion](http://www.cs.berkeley.edu/~bh/ssch11/recursion.html)
; (down 'ringo)
(define (down wd)
 (if (= (count wd) 1)
  (sentence wd)
  (sentence wd (down (butlast wd)))))

; (reverse-word 'ringo)
(define (reverse-word wd)
 (if (= (count wd) 1)
  wd
  (word (last wd) (reverse-word (butlast wd)))))

; (until 'ringo 1)
(define (until wd n)
 (cond
  ((< n 0)
   (reverse-word (until (reverse-word wd) (* n -1))))
  ((> n (count wd))
   wd)
  ((= n 1)
   (first wd))
  ((= n 0)
   wd)
  (else
   (word (first wd) (until (butfirst wd) (- n 1))))))

; (up 'ringo)
(define (up wd)
 (define (inner-up wd n)
  (if (= (count wd) n)
   (sentence wd)
   (sentence (until wd n) (inner-up wd (+ n 1)))))

 (inner-up wd 1))

; (downup 'marsupial)
(define (downup wd)
 (sentence (down wd) (butfirst (up wd))))

; (downup-2 'rin)
; (se 'rin (downup-2 'ri) 'rin)
; (se 'rin (se 'ri (downup-2 'r) 'ri) 'rin)
; (se 'rin (se 'ri (se 'r) 'ri) 'rin)
; (downup-2 'rin)
(define (downup-2 wd)
 (if (= (count wd) 1)
  (se wd)
  (se wd (downup-2 (bl wd)) wd)))

; Exercise 1 - Define describe-time
; (describe-time 22222222)
(define (describe-time secs)
 (cond
  ((< secs 60)
   (sentence secs 'seconds))
  ((< secs 3600)
   (sentence (quotient secs 60) 'minutes (describe-time (remainder secs 60))))
  ((< secs 86400)
   (sentence (quotient secs 3600) 'hours (describe-time (remainder secs 3600))))
  (else
   (sentence (quotient secs 86400) 'days (describe-time (remainder secs 86400))))))

; Exercise 2 - Define remove-once
; (remove-once 'morning '(good morning good morning))
(define (remove-once wd sent)
 (cond
  ((empty? sent)
   (se))
  ((equal? wd (first sent))
   (sentence (butfirst sent)))
  (else
   (sentence (first sent) (remove-once wd (butfirst sent))))))

; Exercise 3 - Define differences
; (differences '(4 23 9 87 6 12))
(define (differences nums)
 (if (= (count nums) 1)
  (sentence)
  (sentence (- (first (butfirst nums)) (first nums))
            (differences (butfirst nums)))))

; Exercise 4 - Define location
;-> (location 'me '(you never give me your money))
;4
;-> (location 'i '(you never give me your money))
;#f
;-> (location 'the '(the fork and the spoon))
;1
;
;
; (location 'i '(you never))
; (if (empty? '(you never)) #f (+ 1 (if (equal? 'i (first '(you never))) 0 (location 'i '(never)))))
; (+ 1 (location 'i '(never)))
; (+ 1 (if (empty? '(never)) #f (+ 1 (if (equal? 'i (first '(never))) 0 (location 'i '())))))
; (+ 1 (+ 1 (location 'i '())))
; (+ 1 (+ 1 #f))
;
(define (location small big)
 (cond
  ((not (member? small big))
   #f)
  ((equal? small (first big))
   1)
  (else
   (+ 1 (location small (butfirst big))))))

; (define (location small big)
;  (if (not (member? small big))
;   #f
;   (+ 1
;      (if (equal? small (first big))
;       0
;       (location small (butfirst big))))))
;
; Exercise 5 - Define initials
; (initials '(if i needed someone))
(define (initials sent)
 (if (empty? sent)
  (sentence)
  (sentence (first (first sent)) (initials (butfirst sent)))))

; Exercise 6 - Define copies
; (copies 8 'spam)
(define (copies num wd)
 (if (= num 0)
  (sentence)
  (sentence wd (copies (- num 1) wd))))

; Exercise 7 - Define gpa
; (base-grade 'E)
(define (base-grade grade)
 (cond
  ((equal? (first grade) (quote A))
   4)
  ((equal? (first grade) (quote B))
   3)
  ((equal? (first grade) (quote C))
   2)
  ((equal? (first grade) (quote D))
   1)
  ((equal? (first grade) (quote F))
   0)))

; (grade-modifier 'B-)
(define (grade-modifier grade)
 (cond
  ((equal? (last grade) (quote -))
   -0.33)
  ((equal? (last grade) (quote +))
   0.33)
  (else
   0)))

; (+ (base-grade 'B+) (grade-modifier 'B-))
(define (inner-gpa grades)
 (if (empty? grades)
  0
  (+ (+ (base-grade (first grades)) (grade-modifier (first grades)))
     (inner-gpa (butfirst grades)))))

; (gpa '(A A+ B+ B))
(define (gpa grades)
 (/ (inner-gpa grades) (count grades)))

; Exercise 8 - Define repeat-words
; (duplicate-word 'samurai 7)
(define (duplicate-word wd n)
 (if (= n 0)
  (sentence)
  (sentence wd (duplicate-word wd (- n 1)))))

; (repeat-words '(4 calling birds 3 french hens))
(define (repeat-words sent)
 (cond
  ((empty? sent)
   (sentence))
  ((number? (first sent))
   (sentence (duplicate-word (first (butfirst sent)) (first sent))
             (repeat-words (butfirst (butfirst sent)))))
  (else
   (sentence (first sent) (repeat-words (butfirst sent))))))

; Exercise 9 - Define same-shape?
; (same-shape? '(the fool on the hill) '(you like me too much))
; (same-shape? '(the fool on the hill) '(and your bird can sing))
(define (same-shape? sent1 sent2)
 (cond
  ((not (= (count sent1) (count sent2)))
   #f)
  ((and (empty? sent1)
        (empty? sent2))
   #t)
  ((not (= (count (first sent1)) (count (first sent2))))
   #f)
  (else
   (and #t
        (same-shape? (butfirst sent1) (butfirst sent2))))))
