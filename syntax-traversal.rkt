#lang racket

(provide search-pos)

; match?: syntax syntax -> boolean
; m: original syntax
; n: syntax to match, may be at-pattern-expr containing '_
; (match? #'(* x (sub1 y)) #'(* x (sub1 y)))
; (match? #'(* x (sub1 y)) #'(* x (sub1 z)))
; (match? #'(* x (sub1 y)) #'(_ _ _))
; (match? #'(* x (sub1 y)) #'_)
(define (match? m n)
  (define (stx-equal? a b)
    (if (and (syntax? a) (syntax? b))
        (let ([a-datum (syntax->datum a)]
              [b-datum (syntax->datum b)])
          (or (equal? b-datum '_)
              (equal? a-datum b-datum)))
        #f))
  (cond
    [(syntax? m)
     (if (stx-equal? m n)
         #t
         (match? (syntax-e m) (syntax-e n)))]
    [(and (pair? m) (pair? n))
     (if (stx-equal? (car m) (car n))
         (match? (cdr m) (cdr n))
         (and (match? (car m) (car n))
              (match? (cdr m) (cdr n))))]
    [else (equal? m n)]))

(define res-pos '())

(define (traverse s c expr before-exprs after-exprs r p)
  (cond
    [(syntax? s)
     (traverse (syntax-e s) c expr before-exprs after-exprs r p)]
    [(pair? s)
     (let* ([first-s (car s)]
            [pos (syntax-position first-s)])
       (case c
         [(before)
          (if (null? before-exprs)
              (traverse s 'target expr before-exprs after-exprs #f p)
              (if (match? first-s (first before-exprs))
                  (traverse (cdr s) 'before expr (rest before-exprs) after-exprs 'before p)
                  (unless (equal? r 'before)
                    (traverse (car s) 'before expr before-exprs after-exprs r p)
                    (traverse (cdr s) 'before expr before-exprs after-exprs r p))))]
         [(target)
          (cond
            [(match? first-s expr)
             (traverse (cdr s) 'after expr before-exprs after-exprs 'target pos)]
            [else
             (traverse (car s) 'target expr before-exprs after-exprs #f p)
             (traverse (cdr s) 'target expr before-exprs after-exprs #f p)])]
         [(after)
          (cond
            [(null? after-exprs)
             (set! res-pos (cons p res-pos))]
            [else
             (if (match? first-s (first after-exprs))
                 (traverse (cdr s) 'after expr before-exprs (rest after-exprs) 'after p)
                 (unless (equal? r 'after) 
                   (traverse (car s) 'after expr before-exprs after-exprs r p)
                   (traverse (cdr s) 'after expr before-exprs after-exprs r p)))])]))]
    [(empty? s)
     (when (and (or (equal? r 'after) (equal? r 'target)) (null? after-exprs))
       (set! res-pos (cons p res-pos)))]))

(define (search-pos s e before after)
  (set! res-pos '())
  (traverse s 'before e before after #f #f)
  res-pos)

#|
; multiple matches
(define s 
  #'(module src racket
      (#%module-begin
       (define (inc-counter)
         (void))
       (define (inc x) (void))
       (define (g)
         (define x (inc 4))
         (inc-counter)
         (+ x 1))
       (define (f)
         (define x (inc 4))
         (inc-counter)
         (+ x 3)))))

(search-pos s #'(inc-counter) (list #'(define x (inc 4))) null)

(search-pos s #'(inc-counter) (list #'(define x (inc 4))) (list #'(+ x 1)))

(search-pos s #'(inc-counter) (list #'(define x (inc 4))) (list #'(+ x 2)))

(search-pos s #'(inc-counter) (list #'(define x (inc 4))) (list #'(+ x 1) #'(+ y 3)))

(search-pos s #'(inc-counter2) (list #'(define x (inc 4))) (list #'(+ x 1)))

(search-pos s #'(inc-counter) null null)

(search-pos s #'(inc-counter) (list #'(define x 3)) null)

(search-pos s #'(inc-counter) (list #'(define x (inc 4)) #'(+ x 1)) null)

(search-pos s #'(inc-counter) null (list #'(+ x 1)))
|#
