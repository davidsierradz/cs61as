#lang racket
(require (planet dyoo/simply-scheme))

(provide (all-defined-out))

;;Begin Project 1
(require "adjectives.rkt")

;;Below is some procedures to help you out. Dont worry about what it says or does.
;;Questions are below.
(define (chatter bot1 bot2 start iterations)
 (define (helper b1 b2 sent i)
  (when (< i iterations)
   (display "bot ")
   (display (add1 (remainder i 2)))
   (display ": ")
   (let ((out (b1 sent)))
    (print-sentence out)
    (helper b2 b1 out (add1 i)))))

 (display "start: ")
 (print-sentence start)
 (helper bot1 bot2 start 0))

(define (interact bot)
 (define (helper)
  (display "> ")
  (flush-output)
  (let ((line (read-line)))
   (unless (want-exit? line)
    (print-sentence (bot line))
    (helper))))

 (read-line)
 (helper))

(define (print-sentence sent)
 (for-each (lambda (x)
            (display x)
            (display " "))
           sent)
 (newline))

(define (want-exit? line)
 (or (member? 'exit line)
     (member? 'quit line)
     (member? 'bye line)))

;;; Checks if a given word is an adjective or not
;;; Requires adjectives.scm to be loaded beforehand
(define adjective?
 (let ((hash (make-hash)))
  (for-each (lambda (adj)
             (hash-set! hash adj #t))
            adjectives)
  (lambda (wd)
   (hash-ref hash wd #f))))

;; Begin Questions:
;;Q1 - babybot
(define (babybot sent)
 sent)

;;Q2 - stupidbot-creator
(define (stupidbot-creator motto)
 (lambda (sent)
  (sentence motto)))

;;Q3 - matcherbot-creator
(define (matcherbot-creator pattern)
 (lambda (sent)
  (cond
   ((se-member? pattern sent)
    (return-after-se-member pattern sent))
   ((not (se-member? pattern sent))
    #f)
   ((equal? (return-after-se-member pattern sent) '()))
   ((equal? pattern '())
    sent))))

(define (return-after-se-member sent1 sent2)
 ; (return-after-se-member '(c d) '(b a c a b c d))
 (cond
  ((< (length sent2) (length sent1))
   '())
  ((equal? sent1 (take sent2 (length sent1)))
   (drop sent2 (length sent1)))
  (else
   (sentence '() (return-after-se-member sent1 (butfirst sent2))))))

(define (se-member? sent1 sent2)
 ; (se-member? '(a a) '(b a c a b c d))
 (cond
  ((< (length sent2) (length sent1))
   #f)
  ((equal? sent1 (take sent2 (length sent1)))
   #t)
  (else
   ; or monoid
   (or #f
       (se-member? sent1 (butfirst sent2))))))

;;Q4 - substitutebot-creator
(define (do-substitution-fold substituters wd)
 ;; (do-substitution-fold (map make-word-substituter '(indonesia winter noodles) '(texas summer steak)) 'other)
 ((foldl compose identity substituters)
  wd))

(define (make-word-substituter from to)
 ; ((make-word-substituter 'indonesia 'texas) 'indonesia)
 (lambda (wd)
  (if (equal? from wd)
   to
   wd)))

(define (make-words-substituter from to)
 ;; ((make-words-substituter '(indonesia winter noodles) '(texas summer steak)) '(I visited indonesia this winter and ate noodles))
 (if (equal? (count from) (count to))
  (lambda (sent)
   (let ((map-substituter (map make-word-substituter from to)))
    (foldl (lambda (cur acc)
            (sentence acc (do-substitution-fold map-substituter cur)))
           '()
           sent)))
  (error "from and to should have the same number of words")))

(define (substitutebot-creator from to)
 ;;insert your answer here
 ;; ((substitutebot-creator '(indonesia winter noodles) '(texas summer steak)) '(I visited indonesia this winter and ate noodles))
 (let ((substituter (make-words-substituter from to)))
  (lambda (sent)
   (substituter sent))))

;;Q5 - switcherbot
(define (do-substitution-fold-switcherbot substituters wd)
 ;; (equal? (do-substitution-fold-switcherbot (map make-word-substituter '(you me) '(me you)) 'you) 'me)
 (cond
  ((empty? substituters)
   wd)
  ((not (equal? ((first substituters)
                 wd)
                wd))
   ((first substituters)
    wd))
  (else
   (do-substitution-fold-switcherbot (butfirst substituters) wd))))

(define (make-words-substituter-switcherbot from to)
 (lambda (sent)
  (let ((map-substituter (map make-word-substituter from to)))
   (foldl
    (lambda (cur acc)
     (sentence acc (do-substitution-fold-switcherbot map-substituter cur)))
    '()
    sent))))

(define (switcherbot sent)
 ;;'(you are smart but I am smarter than you) -> '(I am smart but you are smarter than me)
 ;; (switcherbot '(you are smart but I am smarter than you))
 (let ((substituter
        (make-words-substituter-switcherbot '(me I am was my yours you are)
                                            '(you you are were your mine me am))))
  (if (equal? (first sent) 'you)
   (sentence 'I (substituter (butfirst sent)))
   (substituter sent))))

;;Q6 - inquisitivebot
(define (inquisitivebot sent)
 (let ((substituter
        (make-words-substituter-switcherbot '(me I am was my yours you are)
                                            '(you you are were your mine me am))))
  (if (equal? (first sent) 'you)
   (sentence 'I (substituter (butfirst sent)) '?)
   (sentence (substituter sent) '?))))

;;Q7 - eliza
(define (eliza sent)
 ; (eliza '(I am excited that me will finish unit 1 ?))
 (let ((inquisitivebot
        (lambda (sent)
         (let ((substituter
                (make-words-substituter-switcherbot
                 '(me I am was my yours you are)
                 '(you you are were your mine me am))))
          (if (equal? (first sent) 'you)
           (sentence 'I (substituter (butfirst sent)) '?)
           (sentence (substituter sent) '?)))))
       (switcherbot
        (lambda (sent)
         (let ((substituter
                (make-words-substituter-switcherbot
                 '(me I am was my yours you are)
                 '(you you are were your mine me am))))
          (if (equal? (first sent) 'you)
           (sentence 'I (substituter (butfirst sent)))
           (substituter sent))))))
  (cond
   ((empty? sent)
    '(how can I help you ?))
   ((equal? (first sent) 'hello)
    '(hello there!))
   ((equal? (last sent) '?)
    '(I can not answer your question.))
   ((se-member? '(I am) sent)
    (sentence '(why are you)
              (inquisitivebot (return-after-se-member '(I am) sent))))
   (else
    (switcherbot sent)))))

;;Q8 - reactorbot-creator
(define (reactorbot-creator bot pat out)
 ; (define stupidbot (stupidbot-creator '(I am Groot)))
 ; (define groot (reactorbot-creator stupidbot '(no Groot youll die why are you doing this) '(WE are Groot)))
 ; (groot '(whats up groot))
 ; (groot '(no Groot youll die why are you doing this))
 (lambda (sent)
  (if (se-member? pat sent)
   out
   (bot sent))))

;;Q9 - replacerbot-creator
(define (replacerbot-creator bot pat before after)
 (lambda (sent)
  (cond
   ((se-member? pat sent)
    (sentence before (return-after-se-member pat sent) after))
   (else
    (bot sent)))))

;;Q10 - exagerate
(define (exaggerate bot n)
 ; ONLY BECAUSE I CAN LOL
 ; ((exaggerate babybot 1) '(this soup is hot and tasty))
 ; ((exaggerate babybot 2) '(this soup is hot and tasty))
 (lambda (sent)
  (letrec
   ((response (bot sent))
    (from-one-to-n
     (lambda (sent m n)
      (if (equal? (count sent) n)
       sent
       (sentence (from-one-to-n (sentence sent (+ 1 m)) (+ 1 m) n)))))
    (inner-exaggerate (lambda (sentence-to-exaggerate)
                       (foldl (lambda (cur acc)
                               (if (adjective? cur)
                                (sentence acc 'very cur)
                                (sentence acc cur)))
                              '()
                              sentence-to-exaggerate))))
   (last
    (foldl (lambda (cur acc)
            (if (equal? (first acc) n)
             acc
             (cons (+ 1 (first acc)) (list (inner-exaggerate (last acc))))))
           (cons 0 (list sent))
           (from-one-to-n '() 0 n))))))

;;REMEMBER TO ADD YOUR OWN TESTS TO GRADER.RKT!
;;END OF PROJECT 1
