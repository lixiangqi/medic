#lang racket

(define (fact x a)
  (if (zero? x)
      a
      (fact (sub1 x) (* x a))))

(fact 3 1)
