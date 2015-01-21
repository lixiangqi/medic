#lang racket/base

(provide log 
         aggregate
         edge
         node
         timeline
         assert
         same?)

(define (log datum) (void))

(define (aggregate v . vs) (void))

(define (edge from to [edge-label ""] [color #f] [from-label ""] [to-label ""] ) (void))
(define (node n [node-label ""] [color #f]) (void))

(define (timeline e) (void))

(define (assert cond) (void))

(define (same? e) (void))