#lang racket

(require racket/gui/base
         "tooltip.rkt")

(provide timeline-canvas%)

(define timeline-canvas%
  (class canvas%
    (init-field (data null))
    (inherit get-dc refresh client->screen
             get-top-level-window
             get-view-start
             get-client-size)
    (super-new)
    
    (define labels (map first data))
    (define tags (map second data))
    (define values (map third data))
    (define dc (get-dc))
    
    (define (convert-to-type l)
      (define (is-number-series? l)
        (andmap (lambda (b) (eq? b #t)) (map number? l)))
      (define (is-boolean-series? l)
        (andmap (lambda (b) (eq? b #t)) (map boolean? l)))
      (cond
        [(is-number-series? l) 'number]
        [(is-boolean-series? l) 'boolean]
        [else 'other]))
    
    (define types (map convert-to-type values))
    (define max-label-width (apply max (map (lambda (s) (get-text-width s)) labels)))
    (define max-frame-number (apply max (map length values)))
    
    (define timeline-space 50)
    (define focus -1)
    (define square-size 50)
    (define square-center (/ square-size 2))
    (define start-x #f)
    (define start-y #f)
    (define init-x (+ 2 max-label-width))
    (define init-y square-size)
    (define text-height (cdr (get-text-size "test")))
    (define bitmap-width (inexact->exact (- square-size 4)))
    (define bitmap-height (inexact->exact text-height))
    (define offset (- square-center bitmap-height))
    (define ellipsis-offset (- square-center (/ (get-text-width "...") 2)))
    
    (define single-tooltip (new tooltip-frame% [frame-to-track (get-top-level-window)]))
    (define value-tooltips (make-vector (length labels)))
    (for ([i (in-range (length values))])
      (vector-set! value-tooltips i (new tooltip-frame% [frame-to-track (get-top-level-window)])))
    
    (define/private (get-text-size str)
      (define dc (new bitmap-dc% [bitmap (make-object bitmap% 1 1)]))
      (define-values (w h r1 r2) (send dc get-text-extent str))
      (cons w h))
    
    (define/private (get-text-width s)
      (define-values (w h r1 r2) (send dc get-text-extent s))
      w)
    
    (define/private (visualize-number l)
      
      (define lower-bound (inexact->exact (floor (apply min l))))
      (define upper-bound (inexact->exact (ceiling (apply max l))))
      (define units (- upper-bound lower-bound))
      (define space 6)
      (define ellipse-width 6)
      (define radius (/ ellipse-width 2))
      (define len (length l))
      
      (define (get-line-position x1 y1 x2 y2)
        (define theta (atan (- y2 y1) (- x2 x1)))
        (define sint (sin theta))
        (define cost (cos theta))
        (define m (* radius cost))
        (define n (* radius sint))
        (define line-x1 (+ x1 m))
        (define line-y1 (+ y1 n))
        (define line-x2 (- x2 m))
        (define line-y2 (- y2 n))
        (list line-x1 line-y1 line-x2 line-y2))
      
      (define (draw-gray-background)
        (send dc set-pen "White" 0 'solid)
        (send dc set-brush "LightGray" 'solid)
        (let loop ([i 0])
          (when (< i len)
            (send dc draw-rectangle (+ start-x (* square-size i)) start-y square-size square-size)
            (loop (add1 i)))))
        
      (draw-gray-background)
      (cond 
        [(zero? units)
         (let loop ([i 0])
           (when (< i len)
             (let ([center-x (+ start-x (* square-size i) square-center)]
                   [center-y (+ start-y square-center)])
               (send dc set-pen "DodgerBlue" 1 'solid)
               (send dc draw-ellipse (- center-x radius) (- center-y radius) ellipse-width ellipse-width)
               (when (< i (sub1 len))
                 (send dc draw-line (+ center-x radius) center-y (- (+ center-x square-size) radius) center-y))
               (loop (add1 i)))))]
        [else
         (define unit-length (/ (- square-size space space) units))
         (define plot-heights (map (lambda (v) (* (- v lower-bound) unit-length)) l))
         (let loop ([i 0])
           (when (< i len)
             (let* ([plot-height (list-ref plot-heights i)]
                    [center-x (+ start-x (* square-size i) square-center)]
                    [center-y (+ start-y (- square-size space plot-height))]
                    [ellipse-x (- center-x radius)]
                    [ellipse-y (- center-y radius)])
               (send dc set-pen "DodgerBlue" 1 'solid)
               (send dc draw-ellipse ellipse-x ellipse-y ellipse-width ellipse-width)
               (when (< i (sub1 len))
                 (let* ([next-height (list-ref plot-heights (add1 i))]
                        [next-y (+ start-y (- square-size space next-height))]
                        [line-pos (get-line-position center-x center-y (+ center-x square-size) next-y)])
                   (send dc draw-line (first line-pos) (second line-pos) (third line-pos) (fourth line-pos))))
               (loop (add1 i)))))])
        (send dc set-pen "White" 0 'solid))
    
    (define/private (visualize-boolean l true-color false-color)
      (define len (length l))
      (let loop ([i 0])
        (when (< i len)
          (if (list-ref l i)
              (send dc set-brush true-color 'solid)
              (send dc set-brush false-color 'solid))
          (send dc draw-rectangle (+ start-x (* square-size i)) start-y square-size square-size)
          (loop (add1 i)))))
   
    (define/private (visualize-other-data l)
      (define len (length l))
      (define bm-start-x (+ start-x 2))
      (define bm-start-y (+ start-y offset))
      (define ellipsis-start-x (+ start-x ellipsis-offset))
      (define ellipsis-start-y (+ start-y square-center))
      (let loop ([i 0])
        (when (< i len)
          (let* ([value (format "~v" (list-ref l i))]
                 [m (* square-size i)]
                 [bm (make-object bitmap% bitmap-width bitmap-height #f #t)]
                 [bm-dc (new bitmap-dc% [bitmap bm])]) 
          (send dc set-brush "LightGray" 'solid)
          (send dc draw-rectangle (+ start-x m) start-y square-size square-size)
          (send bm-dc draw-text value 0 0)
          (send dc draw-bitmap bm (+ bm-start-x m) bm-start-y)
          (when (> (car (get-text-size value)) (add1 bitmap-width)) 
            (send dc draw-text "..." (+ ellipsis-start-x m) ellipsis-start-y))
          (loop (add1 i))))))
    
    (define/private (draw-labels)
      (send dc set-text-foreground "Gray")
      (define len (length labels))
      (let loop ([i 0])
        (when (< i len)
          (send dc draw-text (list-ref labels i) 2 (+ square-size offset (* square-size i)))
          (loop (add1 i)))))
    
    (define/private (draw-frame-number)
      (let loop ([i 0])
        (when (< i max-frame-number)
          (send dc draw-text (format "~a" (add1 i)) (+ start-x 2 (* square-size i)) square-center)
          (loop (add1 i)))))
    
    (define/private (display-focus-info)
      (define len (length values))
      (cond
        [(= focus -1)
         (for ([i (in-range len)])
           (send (vector-ref value-tooltips i) show #f))]
        [else
        (let* ([len (length values)]
               [line-x (+ init-x square-center (* square-size focus))]
               [line-y (+ square-size (* len square-size))]
               [tooltip-x (inexact->exact (ceiling (+ line-x 10)))])
          (send dc set-pen "Yellow" 1 'solid)
          (send dc draw-line line-x square-size line-x line-y)
          (define-values (virtual-x virtual-y) (get-view-start))
          (define-values (width height) (get-client-size))
          (for ([i (in-range len)])
            (define current-unit-y (* square-size (add1 i)))
            (cond
              [(or (< current-unit-y virtual-y)
                   (> current-unit-y (+ virtual-y height))
                   (> tooltip-x (+ virtual-x width))
                   (< tooltip-x virtual-x))
                (send (vector-ref value-tooltips i) show #f)]
              [else
               (define series (list-ref values i))
               (if (< focus (length series))
                   (let* ([val (list-ref series focus)]
                          [current-tooltip (vector-ref value-tooltips i)]
                          [tooltip-height (cdr (get-text-size (format "~v" val)))]
                          [tooltip-y (inexact->exact (ceiling (+ current-unit-y (- square-center (/ tooltip-height 2)) (- virtual-y))))])
                     (define-values (canvas-screen-x canvas-screen-y) (client->screen 0 0))
                     (define-values (mx my) (get-display-left-top-inset #:monitor 0))
                     (send current-tooltip show #f)
                     (send current-tooltip set-tooltip (list (format "~v" val)))
                     (send current-tooltip show-over (+ canvas-screen-x tooltip-x (- mx) (- virtual-x)) (+ canvas-screen-y tooltip-y (- my)) 0 0))
                   (send (vector-ref value-tooltips i) show #f))])))]))
      
    (define/override (on-paint)
      (set! start-x init-x)
      (set! start-y init-y)
      (draw-labels)
      (draw-frame-number)
      (send dc set-text-foreground "Black")
      (send dc set-pen "White" 0 'solid)
      (for ([i (in-range (length types))])
        (let ([t (list-ref types i)]
              [d (list-ref values i)]
              [boolean? (list-ref tags i)])
          (case t
            [(number) (visualize-number d)]
            [(boolean) (if boolean? (visualize-boolean d "LightGray" "Red") (visualize-boolean d "Navy" "Red"))]
            [(other) (visualize-other-data d)])
          (set! start-y (+ start-y square-size))))
      (display-focus-info))
    
    (define/private (mouse-position->timeline-value x y)
      (define-values (h v) (get-view-start))
      (define virtual-x (+ h x))
      (define virtual-y (+ v y))
      (define col (inexact->exact (floor (/ (- virtual-x init-x) square-size))))
      (define row (inexact->exact (floor (/ (- virtual-y init-y) square-size))))
      (when (and (>= row 0) (>= col 0)  (< row (length labels)))
        (define series (list-ref values row))
        (when (< col (length series))
          (send single-tooltip show #f)
          (define val (format "~v" (list-ref series col)))
          (define-values (canvas-screen-x canvas-screen-y) (client->screen 0 0))
          (define-values (mx my) (get-display-left-top-inset #:monitor 0))
          (send single-tooltip set-tooltip (list val))
          (send single-tooltip show-over (+ 15 canvas-screen-x x (- mx)) (+ canvas-screen-y y (- my)) 0 0))))
    
    (define/override (on-event evt)
      (cond
        [(send evt button-down? 'left)
         (mouse-position->timeline-value (send evt get-x) (send evt get-y))]
        [(send evt button-up? 'left)
         (send single-tooltip show #f)]))
    
    (define/public (scrutinize cur)
      (set! focus (sub1 cur))
      (unless (= focus -1)
        (define view-x (+ init-x square-center (* square-size focus)))
        (define-values (m n) (get-client-size))
        (when (> view-x m)
          (define p (/ view-x (get-actual-width)))
          (send this scroll p #f)))
      (refresh))
    
    (define/public (get-actual-width)
      (inexact->exact (ceiling (+ max-label-width (* max-frame-number square-size) 5))))
    
    (define/public (get-actual-height)
      (inexact->exact (ceiling (+ (* (add1 (length labels)) square-size) 5))))))