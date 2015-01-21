#lang racket

(define (count-length v count)
  (if (null? v)
      count
      (count-length (cdr v) (+ count 1))))

(count-length (cons 8 (cons 9 '())) 0)