#lang racket

(define counter 0)

(define (inc-counter) (set! counter (add1 counter)))

(define (inc x) 
  (inc-counter)
  (+ x 1))

(define (g x)
  (inc x))

(g 4)
