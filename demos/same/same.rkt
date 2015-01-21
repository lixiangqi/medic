#lang racket

(define x (vector 1 2 3 4 5))
(define y (list 1 -2 3 4 -5))

(define (convert-to-vector v)
  (cond
    [(vector? v) v]
    [(list? v) (apply vector v)]))

(define (make-new-vector v)
  (vector-set! (convert-to-vector v) 0 #f))

(make-new-vector x)
(make-new-vector y)