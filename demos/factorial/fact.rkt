#lang racket

(define (fact x)
  (if (zero? x)
      1
      (* x (fact (- x 1)))))

(fact 3)