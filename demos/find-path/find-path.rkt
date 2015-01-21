#lang racket
 
(define (find-path t name)
  (cond
    [(string? t) (if (equal? t name) '() #f)]
    [else
     (let ([left-p (find-path (cadr t) name)])
       (if left-p
           (cons (car t) left-p)
           (let ([right-p (find-path (caddr t) name)])
             (if right-p
                 (cons (car t) right-p)
                 #f))))]))
 
(find-path '("a" ("b" "1" "2") ("c" "3" "4")) "3")