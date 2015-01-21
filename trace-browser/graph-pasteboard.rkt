#lang racket

(require mrlib/graph
         racket/gui/base
         (only-in racket/string string-normalize-spaces)
         (only-in unstable/gui/pict dark light)
         "quadtree.rkt")

(provide graph-pasteboard%)

(define node-size 20)
(define radius (/ node-size 2))

(define (create-node-bitmap label color w)
  (define bm (make-object bitmap% (inexact->exact (ceiling (+ w w node-size))) node-size #f #t))
  (define bm-dc (new bitmap-dc% [bitmap bm]))
  (send bm-dc set-brush (or color "DodgerBlue") 'solid)
  (send bm-dc set-pen "Light Gray" 0 'solid)
  (send bm-dc draw-ellipse w 0 node-size node-size)
  (send bm-dc draw-text label (+ w node-size) 0)
  bm)

(define graph-editor-snip%
  (class (graph-snip-mixin editor-snip%)
    (field [label-width #f])
    (super-new)
    (define/public (get-label-width) label-width)
    (define/public (set-label-width w) (set! label-width w))))

(define graph-pasteboard%
  (class (graph-pasteboard-mixin pasteboard%)
    (init-field [raw-nodes #f]
                [raw-edges #f]
                [width #f]
                [height #f])
    (inherit insert
             begin-edit-sequence
             end-edit-sequence
             set-selection-visible)
    
    (define friction 0.9)
    (define charge -400.0)
    (define gravity 0.1)
    (define theta2 0.64)
    (define link-distance 100.0)
    (define link-strength 1.0)
    (define charge-distance2 +inf.0)
    (define alpha 0.1)
    (define neighbors (make-hash))
    
    (define snips (make-hash))
    (define nodes (make-hash))
    (define edges (make-hash))
    
    (define/augment (can-interactive-resize? evt) #f)
    
    (define/private (init-graph-elements)
      (for-each (lambda (i)
                  (let ([n (first i)]
                        [label (second i)]
                        [color (third i)])
                    (unless (hash-has-key? nodes n)
                      (hash-set! nodes n (new node% [label label] [color color])))))
                raw-nodes)
       
      (for-each
       (lambda (key)
         (let* ([from (car key)]
                [to (cdr key)]
                [val (hash-ref raw-edges key)]
                [edge-label (first val)]
                [from-label (second val)]
                [to-label (third val)]
                [bi-directed? (fourth val)]
                [color (fifth val)])
           (unless (hash-has-key? nodes from) (hash-set! nodes from (new node% [label from-label])))
           (unless (hash-has-key? nodes to) (hash-set! nodes to (new node% [label to-label])))
           (define from-node (hash-ref nodes from))
           (define to-node (hash-ref nodes to))
           (unless (equal? from-node to-node)
             (let ([k (cons from to)])
               (unless (hash-has-key? edges k)
                 (hash-set! edges k (new edge% 
                                         [from from-node] 
                                         [to to-node] 
                                         [label edge-label] 
                                         [bi-directed bi-directed?]
                                         [color color])))))))
         (hash-keys raw-edges)))
   
    (define node%
      (class object%
        (init-field [label #f]
                    [color #f])
        (field [x +nan.0]
               [y +nan.0]
               [px +nan.0]
               [py +nan.0]
               [weight 0.0]
               [fixed #f])
        (super-new)
        (install-node this)
        
        (define/public (get-x) x)
        (define/public (set-x new-x) (set! x new-x))
        
        (define/public (get-y) y)
        (define/public (set-y new-y) (set! y new-y))
        
        (define/public (get-px) px)
        (define/public (set-px new-px) (set! px new-px))
        
        (define/public (get-py) py)
        (define/public (set-py new-py) (set! py new-py))
        
        (define/public (fixed?) fixed)
        (define/public (get-label) label)
        (define/public (get-color) color)
        (define/public (get-weight) weight)
        (define/public (incr-weight) (set! weight (add1 weight)))))
    
    (define edge%
      (class object%
        (init-field [from #f]
                    [to #f]
                    [label ""]
                    [bi-directed #f]
                    [color #f])
    
        (super-new)
        (install-edge this)
    
        (define/public (get-from-node) from)
        (define/public (set-from-node f) (set! from f))
        
        (define/public (get-to-node) to)
        (define/public (set-to-node t) (set! to t))
    
        (define/public (get-label) label)
        (define/public (get-bi-directed) bi-directed)
        (define/public (get-color) color)))
    
    (define/private (get-text-width str)
      (define dc (new bitmap-dc% [bitmap (make-object bitmap% 1 1)]))
      (define-values (w h r1 r2) (send dc get-text-extent str))
      w)
    
    (define/private (install-node n)
      (when (nan? (send n get-x)) (send n set-x (position n 0)))
      (when (nan? (send n get-y)) (send n set-y (position n 1)))
      (when (nan? (send n get-px)) (send n set-px (send n get-x)))
      (when (nan? (send n get-py)) (send n set-py (send n get-y)))
      (let* ([text (new text%)]
             [s (new graph-editor-snip% [editor text])]
             [label (send n get-label)]
             [width (get-text-width label)]
             [color (send n get-color)]
             [image-snip (make-object image-snip% (create-node-bitmap label color width))])
        (send text begin-edit-sequence)
        (send text insert image-snip)
        (send text end-edit-sequence)
        (send text lock #t)
        (send s show-border #f)
        (send s use-style-background #t)
        (send s set-label-width width)
        (hash-set! snips n s)))
    
    (define/private (install-edge e)
      (define from (send e get-from-node))
      (define to (send e get-to-node))
      (send from incr-weight)
      (send to incr-weight)
      (define from-neighbor (hash-ref neighbors from '()))
      (define to-neighbor (hash-ref neighbors to '()))
      (hash-set! neighbors from (cons to from-neighbor))
      (hash-set! neighbors to (cons from to-neighbor)))
    
    (define/public (layout-nodes)
      (let loop ([t (tick)])
        (when t
          (loop (tick))))
      (begin-edit-sequence)
      (for-each
       (lambda (n)
         (define snip (hash-ref snips n))
         (define lw (send snip get-label-width))
         (insert (hash-ref snips n) (- (send n get-x) (/ (+ lw node-size) 2)) (- (send n get-y) radius)))
       (hash-values nodes))
      (end-edit-sequence))
    
    (define/public (layout-edges)
      (define default-light-color "LightGray")
      (define default-dark-color "Gray")
      (for-each
       (lambda (e)
         (let* ([from-node (send e get-from-node)]
                [to-node (send e get-to-node)]
                [color (send e get-color)]
                [light-color (or color default-light-color)]
                [dark-color (if color (dark light-color) default-dark-color)]
                [light-pen (send the-pen-list find-or-create-pen default-light-color 0 'solid)]
                [dark-pen (send the-pen-list find-or-create-pen default-dark-color 0 'solid)]
                [light-brush (send the-brush-list find-or-create-brush light-color 'solid)]
                [dark-brush (send the-brush-list find-or-create-brush dark-color 'solid)]
                [light-c (make-object color% default-light-color)]
                [dark-c (make-object color% default-dark-color)])
           (add-links/text-colors (hash-ref snips from-node)
                                  (hash-ref snips to-node)
                                  dark-pen light-pen
                                  dark-brush light-brush
                                  dark-c light-c
                                  0 0
                                  (send e get-label))))
       (hash-values edges)))
    
    (define/public (tick)
      (set! alpha (* alpha 0.99))
      (if (< alpha 0.005)
          #f
          (let ([k 0.0]
                [x 0.0]
                [y 0.0]
                [graph-nodes (hash-values nodes)])
            (for-each
             (lambda (e)
               (let* ([from (send e get-from-node)]
                      [to (send e get-to-node)]
                      [from-x (send from get-x)]
                      [from-y (send from get-y)]
                      [to-x (send to get-x)]
                      [to-y (send to get-y)])
                 (set! x (- to-x from-x))
                 (set! y (- to-y from-y))
                 (define l (+ (sqr x) (sqr y)))
                 (unless (zero? l)
                   (set! l (sqrt l))
                   (set! l (/ (* alpha link-strength (- l link-distance)) l))
                   (set! x (* x l))
                   (set! y (* y l))
                   (set! k (/ (send from get-weight) 
                                (+ (send to get-weight) (send from get-weight))))
                   (send to set-x (- to-x (* x k)))
                   (send to set-y (- to-y (* y k)))
                   (set! k (- 1 k))
                   (send from set-x (+ from-x (* x k)))
                   (send from set-y (+ from-y (* y k))))))
             (hash-values edges))
            (set! k (* alpha gravity))
            (unless (zero? k)
              (set! x (/ width 2.0))
              (set! y (/ height 2.0))
              (for-each
               (lambda (n)
                 (let ([n-x (send n get-x)]
                       [n-y (send n get-y)])
                 (send n set-x (+ n-x (* k (- x n-x))))
                 (send n set-y (+ n-y (* k (- y n-y))))))
               graph-nodes))
            (unless (zero? charge)
              (define qtree (new quadtree% [data graph-nodes]))
              (define qnode (send qtree get-root))
              (force-accumulate qnode)
              (for-each (lambda (n)
                          (unless (send n fixed?) (send qtree visit (repulse n))))
                        graph-nodes))
            (for-each 
             (lambda (n)
               (cond
                 [(send n fixed?)
                  (send n set-x (send n get-px))
                  (send n set-y (send n get-py))]
                 [else
                  (define node-x (send n get-x))
                  (define node-y (send n get-y))
                  (define temp-x node-x)
                  (define temp-y node-y)
                  (define node-px (send n get-px))
                  (define node-py (send n get-py))
                  (send n set-x (- node-x (* (- node-px node-x) friction)))
                  (send n set-px temp-x)
                  (send n set-y (- node-y (* (- node-py node-y) friction)))
                  (send n set-py temp-y)]))
             graph-nodes)
            #t)))
    
    ;; Supports stable incremental layout. When a new node is added and if a linked node
    ;; already has an initial position, the corresponding coordinates are applied
    ;; to the new node, rather than generating random position.
    (define/private (position n d)
      (define ret #f)
      (when (hash-has-key? neighbors n)
        (for-each 
         (lambda (node)
           (let ([n-x (send node get-x)]
                 [n-y (send node get-y)])
             (when (and (zero? d) (not (nan? n-x))) (set! ret n-x))
             (when (and (not ret) (= d 1) (not (nan? n-y))) (set! ret n-y))))
         (hash-ref neighbors n)))
      (or ret (* (random) (if (zero? d) width height)))) 
    
    (define/public (repulse node)
      (lambda (quad x1 y1 x2 y2)
        (define flag #f)
        (when (or (not (send quad get-point))
                         (not (eq? (send quad get-point) node)))
          (let* ([dx (- (send quad get-cx) 
                        (send node get-x))]
                 [dy (- (send quad get-cy) (send node get-y))]
                 [dw (- x2 x1)]
                 [dn (+ (sqr dx) (sqr dy))])
            (when (< (/ (sqr dw) theta2) dn)
              (when (< dn charge-distance2)
                (let ([k (/ (send quad get-charge) dn)])
                  (send node set-px (- (send node get-px) (* dx k)))
                  (send node set-py (- (send node get-py) (* dy k)))))
              (set! flag #t))
            (when (and flag (send quad get-point)
                       (not (zero? dn))
                       (< dn charge-distance2))
              (let ([k (/ (send quad get-point-charge) dn)])
                (send node set-px (- (send node get-px) (* dx k)))
                (send node set-py (- (send node get-py) (* dy k)))))))
        (or flag
            (< (send quad get-charge) 0.00005))))
    
    (define/public (force-accumulate quad)
      (define cx 0.0)
      (define cy 0.0)
      (send quad set-charge 0.0)
      (unless (send quad get-leaf)
        (let ([qt-nodes (send quad get-nodes)])
          (for ([i (in-range (vector-length qt-nodes))])
            (let ([n (vector-ref qt-nodes i)])
              (when n
                (let ([node-charge (send n get-charge)])
                  (force-accumulate n)
                  (send quad set-charge (+ (send quad get-charge) node-charge))
                  (set! cx (+ cx (* node-charge (send n get-cx))))
                  (set! cy (+ cy (* node-charge (send n get-cy))))))))))
      (when (send quad get-point)
        (define graph-node (send quad get-point))
        (when (not (send quad get-leaf))
          (send graph-node set-x (+ (send graph-node get-x) (- (random) 0.5)))
          (send graph-node set-y (+ (send graph-node get-y) (- (random) 0.5))))
        (let ([k (* alpha charge)])
          (send quad set-point-charge k)
          (send quad set-charge (+ (send quad get-charge) (send quad get-point-charge)))
          (set! cx (+ cx (* k (send graph-node get-x))))
          (set! cy (+ cy (* k (send graph-node get-y))))))
      (send quad set-cx (/ cx (send quad get-charge)))
      (send quad set-cy (/ cy (send quad get-charge))))
    
    (super-new)
    (set-selection-visible #f)
    (init-graph-elements)
    (layout-nodes)
    (layout-edges)))