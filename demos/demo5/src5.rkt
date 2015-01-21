#lang racket

(require "f.rkt")

(define t 5)

(define (g x)
  (* x (f x t)))

(g 3)