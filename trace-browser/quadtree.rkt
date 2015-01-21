#lang racket

(provide quadtree-node%
         quadtree%)

(define quadtree-node%
  (class object%
    (field [leaf #t]
           [nodes (make-vector 4 #f)]
           [point #f]
           [x +nan.0]
           [y +nan.0]
           [charge +nan.0]
           [cx +nan.0]
           [cy +nan.0]
           [point-charge +nan.0])
    
    (define/public (get-leaf) leaf)
    (define/public (set-leaf l) (set! leaf l))
    
    (define/public (get-x) x)
    (define/public (set-x new-x) (set! x new-x))
    
    (define/public (get-y) y)
    (define/public (set-y new-y) (set! y new-y))
    
    (define/public (get-cx) cx)
    (define/public (set-cx x) (set! cx x))
    
    (define/public (get-cy) cy)
    (define/public (set-cy y) (set! cy y))
    
    (define/public (get-point) point)
    (define/public (set-point p) (set! point p))
    
    (define/public (get-charge) charge)
    (define/public (set-charge c) (set! charge c))
    
    (define/public (get-point-charge) point-charge)
    (define/public (set-point-charge c) (set! point-charge c))
    
    (define/public (get-nodes) nodes)
    
    (super-new)))

(define quadtree%
  (class object%
    (init-field [data null])
    (field [x1 +inf.0]
           [y1 +inf.0]
           [x2 -inf.0]
           [y2 -inf.0]
           [xs null]
           [ys null]
           [root (new quadtree-node%)])
    
    (define/public (get-root) root)
    
    ;; Recursively inserts the specified point p at the Quadtree node n or one of its descendants
    ;; The bounds are specified by [x1, x2] and [y1, y2]
    ;; n: quadtree-node, v: graph node
    (define/public (insert n v x y x1 y1 x2 y2)
      (cond 
        [(or (nan? x) (nan? y)) #f]
        [(send n get-leaf)
         (define nx (send n get-x))
         (define ny (send n get-y))
         (if (nan? nx)
             (begin
               (send n set-x x)
               (send n set-y y)
               (send n set-point v))
             ; If the point at this leaf node is at the same position as the new
             ; point we are adding, we leave the point associated with the
             ; internal node while adding the new point to a child node. This
             ; avoids infinite recursion.
             (if (< (+ (abs (- nx x)) (abs (- ny y))) 0.01)
                 (insert-child n v x y x1 y1 x2 y2)
                 (let ([npoint (send n get-point)])
                   (send n set-point #f)
                   (send n set-x +nan.0)
                   (send n set-y +nan.0)
                   (insert-child n npoint nx ny x1 y1 x2 y2)
                   (insert-child n v x y x1 y1 x2 y2))))]
        [(not (send n get-leaf))
         (insert-child n v x y x1 y1 x2 y2)]))
    
    ;; Recursively inserts the specified point [x, y] into a descendant of node n.
    ;; The bounds are defined by [x1, x2] and [y1, y2].
    ;; n: quadtree-node, v: graph node
    (define/public (insert-child n v x y x1 y1 x2 y2)
      (define sx (* (+ x1 x2) 0.5))
      (define sy (* (+ y1 y2) 0.5))
      (define right (>= x sx))
      (define bottom (>= y sy))
      (define i (+ (arithmetic-shift (if bottom 1 0) 1) (if right 1 0)))
      ; recursively insert into the child node
      (send n set-leaf #f)
      (define nodes (send n get-nodes))
      (unless (vector-ref nodes i) (vector-set! nodes i (new quadtree-node%)))
      ; update the bounds
      (if right 
          (set! x1 sx)
          (set! x2 sx))
      (if bottom
          (set! y1 sy)
          (set! y2 sy))
      (insert (vector-ref nodes i) v x y x1 y1 x2 y2))
    
    (define/public (visit f) (quadtree-visit f root x1 y1 x2 y2))
    
    (define/private (quadtree-visit f node x1 y1 x2 y2)
      (unless (f node x1 y1 x2 y2)
        (let* ([sx (* (+ x1 x2) 0.5)]
               [sy (* (+ y1 y2) 0.5)]
               [children (send node get-nodes)]
               [c0 (vector-ref children 0)]
               [c1 (vector-ref children 1)]
               [c2 (vector-ref children 2)]
               [c3 (vector-ref children 3)])
          (when c0 (quadtree-visit f c0 x1 y1 sx sy))
          (when c1 (quadtree-visit f c1 sx y1 x2 sy))
          (when c2 (quadtree-visit f c2 x1 sy sx y2))
          (when c3 (quadtree-visit f c3 sx sy x2 y2)))))
    
    (super-new)
    (for-each
     (lambda (n)
       (let ([nx (send n get-x)]
             [ny (send n get-y)])
         (when (< nx x1) (set! x1 nx))
         (when (< ny y1) (set! y1 ny))
         (when (> nx x2) (set! x2 nx))
         (when (> ny y2) (set! y2 ny))
         (set! xs (cons nx xs))
         (set! ys (cons ny ys))))
     data)
    (set! xs (reverse xs))
    (set! ys (reverse ys))
    ; squarify bounds
    (define dx (- x2 x1))
    (define dy (- y2 y1))
    (if (> dx dy)
        (set! y2 (+ y1 dx))
        (set! x2 (+ x1 dy)))
    (for ([i (in-range (length data))])
      (insert root (list-ref data i) (list-ref xs i) (list-ref ys i) x1 y1 x2 y2))
    ; discard captured fields
    (set! xs null)
    (set! ys null)))
    
    
    
    
    
           
    
                