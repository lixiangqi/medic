#lang racket

(provide f)

(define (f x y)
  (+ (sqr x) (sqr y)))