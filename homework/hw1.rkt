#lang racket
(require berkeley)

(provide (all-defined-out))

; Exercise 1 - Define dupls-removed
; (dupls-removed '(a b c a e d e b))
(define (dupls-removed sent)
 (cond
  ((empty? sent)
   '())
  ((member? (first sent) (butfirst sent))
   (dupls-removed (butfirst sent)))
  (else
   (sentence (first sent) (dupls-removed (butfirst sent))))))

; Exercise 2 - Define count-word
;; This should output 2
;(count-word '(i really really like 61as) 'really)
;; This should output 0
;(count-word '(i lambda racket) 'love)
(define (count-word sent wd)
 (cond
  ((not (member? wd sent))
   0)
  ((= (count sent) 0)
   0)
  ((equal? wd (first sent))
   (+ 1 (count-word (butfirst sent) wd)))
  (else
   (count-word (butfirst sent) wd))))

; Exercise 3
(define (pigl wd)
 (if (pl-done? wd)
  (word wd 'ay)
  (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
 (vowel? (first wd)))

(define (vowel? letter)
 (member? letter '(a e i o u)))

; Explain what would happen if you used new-if instead of if below.
#|
Your explanation here
If "if" wasn't a special-form, we would have to apply normal evaluation rules
for the two branches. So we would get an error in "pl-done?" when "wd" is empty `""`
|#

; Exercise 4 - Define squares
;; This should output (1 4 9)
; (squares '(1 2 3))
(define (square x)
 (* x x))

(define (squares sent)
 (if (empty? sent)
  '()
  (sentence (square (first sent)) (squares (butfirst sent)))))

; Exercise 5 - Define switch
; (switch-helper 'bar)
(define (switch-helper wd)
 (cond
  ((or (equal? wd 'I)
       (equal? wd 'me))
   'you)
  ((equal? wd 'you)
   'me)
  (else
   wd)))

;; (switch-inner '(you told me that I should wake you up))
(define (switch-inner sent)
 (if (empty? sent)
  '()
  (sentence (switch-helper (first sent)) (switch-inner (butfirst sent)))))

;; (switch '(you told me that I should wake you up))
;; (switch (quote (I love her)))
(define (switch sent)
 (sentence (if (equal? (first sent) 'you)
            'I
            (switch-helper (first sent)))
           (switch-inner (butfirst sent))))

; Exercise 6 - Define ordered?
; and monoid
(define (ordered? sent)
 (if (= (count sent) 1)
  #t
  (and (if (< (first sent) (first (butfirst sent)))
        #t
        #f)
       (ordered? (butfirst sent)))))

; Exercise 7 - Define ends-e
; (ends-e '(please put the salami above the blue elephant))
(define (ends-e sent)
 (cond
  ((empty? sent)
   '())
  ((equal? 'e (last (first sent)))
   (sentence (first sent) (ends-e (butfirst sent))))
  (else
   (ends-e (butfirst sent)))))

; Exercise 8
#|

Your explanation here

|#
