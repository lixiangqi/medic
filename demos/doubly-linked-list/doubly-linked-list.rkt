#lang racket

(define node%
  (class object%
    (super-new)
    (init-field [datum 0])
    (field [next #f]
           [previous #f])))

(define doubly-linked-list%
  (class object%
    (field [head #f]
           [tail #f])
    (super-new)
    (define size 0)
    
    (define/public (init d)
      (set! head (new node% [datum d]))
      (set! tail head)
      (set! size 1))
    
    (define/public (element-at i)
      (when (or (> i (sub1 size)) (< i 0))
        (error 'element-at-invalid-argument))
      (define temp head)
      (let loop ()
        (when (not (zero? i))
          (set! temp (get-field next temp))
          (set! i (sub1 i))
          (loop)))
      (get-field datum temp))
    
    (define/public (get-size) size)
    
    (define/public (add d)
      (cond
        [(zero? size) (init d)]
        [else
         (define temp (new node% [datum d]))
         (set-field! previous temp tail)
         (set-field! next tail temp)
         (set! tail temp)
         (set! size (add1 size))]))
    
    (define/public (add-at i d)
      (when (or (< i 0) (> i size))
        (error 'add-invalid-arguments))
      (if (= i size)
          (add d)
          (cond
            [(zero? i)
             (define temp (new node% [datum d]))
             (set-field! next temp head)
             (set-field! previous head temp)
             (set! head temp)
             (set! size (add1 size))]
            [else
             (define temp (new node% [datum d]))
             (define p head)
             (for ([j (in-range i)])
               (set! p (get-field next p)))
             (set-field! next temp p)
             (define p-prev (get-field previous p))
             (set-field! previous temp p-prev)
             (set-field! next p-prev temp)
             (set-field! previous p temp)
             (set! size (add1 size))])))
    
    (define/public (remove i)
      (when (or (< i 0) (> i (sub1 size)))
        (error 'remove-invalid-argument))
      (cond
        [(zero? i)
         (define res (get-field datum head))
         (set! head (get-field next head))
         (if head
             (set-field! previous head #f)
             (set! tail #f))
         (set! size (sub1 size))
         res]
        [else
         (cond
           [(= i (sub1 size))
            (define res (get-field datum tail))
            (set! tail (get-field previous tail))
            (set-field! next tail #f)
            (set! size (sub1 size))
            res]
           [else
            (define temp head)
            (for ([j (in-range i)]) (set! temp (get-field next temp)))
            (define res (get-field datum temp))
            (define temp-prev (get-field previous temp))
            (define temp-next (get-field next temp))
            (set-field! next temp-prev temp-next)
            (set-field! previous temp-next temp-prev)
            (set! size (sub1 size))
            res])]))))