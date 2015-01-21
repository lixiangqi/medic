#lang racket

(define x 10)

(define counter 0)

(define (inc-counter) (set! counter (add1 counter)))

(define (inc x) 
  (inc-counter)
  (+ x 1))

(define (g)
  (define x (inc 4))
  (inc-counter)
  (+ x 1))

(g)
