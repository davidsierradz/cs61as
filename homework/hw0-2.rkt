#lang racket
(require berkeley)

(provide (all-defined-out))

; Exercise 1
(define (second wd)
 (first (bf wd)))

; 1. Define first-two
(define (first-two wd)
 (word (first wd) (second wd)))

; ;2. Define two-first
(define (two-first x y)
 (word (first x) (first y)))

; ;3. Define two-first-sent
(define (two-first-sent sent)
 (word (first (first sent)) (first (second sent))))

; Exercise 2 - Define teen?
(define (teen? num)
 (if (and (>= num 13)
          (<= num 19))
  #t
  #f))

; Exercise 3 - Define indef-article
(define (indef-article wd)
 (sentence (if (member? (first wd) 'aeiou)
            'an
            'a)
           wd))

; Exercise 4 - Define insert-and
(define (insert-and sent)
 (sentence (butlast sent) 'and (last sent)))

; Exercise 5 - Define query
(define (query sent)
 (sentence (second sent)
           (first sent)
           (butfirst (butfirst (butlast sent)))
           (word (last sent) '?)))

; Exercise 6 - Define european-time and american-time
(define (european-time time)
 (if (equal? (last time) 'am)
  (if (equal? (first time) 12)
   0
   (first time))
  (if (equal? (first time) 12)
   12
   (+ (first time) 12))))

; (european-time '(8 am))
; (european-time '(4 pm))
; (european-time '(12 am))
; (european-time '(12 pm))
(define (american-time time)
 (cond
  ((= time 0)
   (sentence 12 'am))
  ((and (>= time 1)
        (<= time 11))
   (sentence time 'am))
  ((= time 12)
   (sentence 12 'pm))
  (else
   (sentence (- time 12) 'pm))))

; (american-time 21)
; (american-time 24)
; (american-time 12)
; (american-time 1)
; (american-time 0)
; Exercise 7 - Define describe-time
(define (describe-time secs)
 (cond
  ((and (>= secs 60)
        (< secs 3600))
   (sentence (/ secs 60.0) 'minutes))
  ((and (>= secs 3600)
        (< secs 86400))
   (sentence (/ secs 3600.0) 'hours))
  ((and (>= secs 86400)
        (< secs 31557600))
   (sentence (/ secs 86400.0) 'days))
  (else
   (sentence secs 'seconds))))

; (/ 930 60)
; (* 86400 365.25)
; (describe-time 45)
; (describe-time (* 86400 365))
; Exercise 8 - Explain why superlative doesnt work:
(define (superlative adjective wd)
 (se (word adjective 'est) wd))

; (superlative 'dumb 'exercise)
