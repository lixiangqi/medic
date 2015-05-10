#lang racket

(require racket/draw
         racket/gui/base)

(provide time-slider%)

(define time-slider%
  (class canvas%
    (init-field [max 1]
                [slider-width 200]
                [timeline-canvas #f])
    (inherit get-dc
             refresh
             get-parent)
    (super-new)
    
    (define slider-color "LightGray")
    (define thumb-color "Green")
    (define thumb-light-color "Green")
    (define thumb-dark-color "SeaGreen")
    
    (define margin-space 10)
    (define rect-height 6)
    (define diameter 16)
    (define radius (/ diameter 2))
    (define thumb-top-y (- margin-space (- radius (/ rect-height 2))))
    (define thumb-bottom-y (+ thumb-top-y diameter))
    (define thumb-left-x margin-space)
    (define thumb-right-x (+ thumb-left-x diameter))
    (define label-y (+ thumb-bottom-y 5))
    (define scale 1)
    (define rect-width (- slider-width (+ margin-space margin-space diameter)))
    (define max-width (+ rect-width margin-space))
    (define unit-val (/ max (* rect-width 1.0)))
    
    (define thumb-dragged? #f)
    (define thumb-val 0)
    
    (send timeline-canvas update-view thumb-val)
    
    (define dc (get-dc))
    
    (define/private (draw-thumb)
      (send dc set-pen (send the-pen-list find-or-create-pen thumb-color 0 'solid))
      (send dc set-brush (send the-brush-list find-or-create-brush thumb-color 'solid))
      (send dc draw-ellipse thumb-left-x thumb-top-y diameter diameter))
    
    (define/override (on-paint)
      (send dc set-pen (send the-pen-list find-or-create-pen slider-color 0 'solid))
      (send dc set-brush (send the-brush-list find-or-create-brush slider-color 'solid))
      (send dc draw-rounded-rectangle margin-space margin-space rect-width rect-height)
      (draw-thumb)
      (send dc draw-text (format "~a" (truncate-val (* thumb-val scale))) (+ thumb-left-x radius) label-y))
    
    (define/private (on-thumb? x y)
      (and (>= x thumb-left-x) (<= x thumb-right-x)
           (>= y thumb-top-y) (<= y thumb-bottom-y)))
    
    (define/private (set-thumb-dragged b)
      (if b
          (set! thumb-color thumb-dark-color)
          (set! thumb-color thumb-light-color))
      (set! thumb-dragged? b))
    
    (define (truncate-val val)
      (define (round-f a)
        (define ceil (exact-ceiling a))
        (define diff (- ceil a))
        (if (> diff 0.5)
            (exact-floor a)
            ceil))
      (/ (round-f (* val 100)) 100.0))
    
    (define/override (on-event event)
      (define mouse-x (send event get-x))
      (define mouse-y (send event get-y))
      (cond
        [(send event button-down? 'left)
         (when (on-thumb? mouse-x mouse-y)
           (set-thumb-dragged #t))]
        
        [(send event dragging?)
         (when thumb-dragged?
           (when (and (>=  mouse-x margin-space) (<= mouse-x max-width))
             (set! thumb-val (truncate-val (* (- mouse-x margin-space) unit-val)))
             (set! thumb-left-x (- mouse-x radius))
             (set! thumb-right-x (+ thumb-left-x diameter))
             (send timeline-canvas update-view thumb-val)
             (refresh)))]
        
        [(send event button-up? 'left)
         (when thumb-dragged? 
           (set-thumb-dragged #f)
           (refresh))]))
    
    (define/public (adjust-slider-width w) 
      (set! slider-width w)
      (set! rect-width (- slider-width (+ margin-space margin-space diameter)))
      (set! unit-val (/ max (* rect-width 1.0)))
      (refresh))
    
    (define/public (adjust-range s) (set! scale s) (refresh))
    
    (define/public (get-value) thumb-val)))