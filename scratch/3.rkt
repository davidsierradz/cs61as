(define (count-stairs n)
 ; (count-stairs 5)
 (cond
  ((= n 1)
   1)
  ((= n 2)
   2)
  (else
   (+ (count-stairs (- n 1)) (count-stairs (- n 2))))))
