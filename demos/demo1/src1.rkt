#lang racket

(define z 2)
(define n 9)

(define (f x)
  (define z 5)
  (define n 4)
  (if (zero? x)
      1
      (* x (sub1 x))))

(f 3)