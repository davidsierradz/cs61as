#lang racket

(define the-continuation #f)

(define (test)
        (let ((i 0))
             (call/cc (lambda (k) (set! the-continuation k)))
             (set! i (+ i 1))
             i))
