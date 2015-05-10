#lang racket

(require racket/class
         racket/gui/base
         framework
         "time-slider.rkt"
         "timeline-canvas.rkt")

(provide time-frame%)

(define time-frame%
  (class (frame:basic-mixin frame%)
    (init-field (data #f)
                (max-time #f))
    (inherit get-area-container)
    (super-new (label "Time Viewer")
               (width 400)
               (height 400))
    
    (define unit 1)
    
    (define/private (update-time-unit c event)
      (define val (send c get-value))
      (cond
        [(equal? val "1s") (set! unit 0.001)]
        [(equal? val "1ms") (set! unit 1)]
        [(equal? val "0.1ms") (set! unit 10)])
      (send slider adjust-range unit))
    
    (define (filter-time slider event)
      (void))
    
    (define panel (new vertical-panel%
                       [parent (get-area-container)]
                       [stretchable-height #t]))
    (define timeline-canvas #f)
    
    (when data
      (set! timeline-canvas (new timeline-canvas%
                                 [data data]
                                 [parent panel]
                                 [style '(hscroll vscroll)])))
    
    (send timeline-canvas init-auto-scrollbars 
          (send timeline-canvas get-actual-width) 
          (send timeline-canvas get-actual-height)
          0.0 0.0)
    (send timeline-canvas show-scrollbars #t #t)
    
    (define slider-panel (new horizontal-panel% 
                              [parent panel] 
                              [stretchable-height #f]))
    
    (define slider (new time-slider% 
                        [parent slider-panel]
                        [min-height 42]
                        [stretchable-height #f]
                        [max max-time]
                        [slider-width 250]
                        [timeline-canvas timeline-canvas]))
    (define combo-field (new combo-field%
                             [label "unit:"]
                             [parent slider-panel]
                             [min-width 120]
                             [min-height 42]
                             [stretchable-width #f]
                             [choices (list "1s" "1ms" "0.1ms")]
                             [init-value "1ms"]
                             [callback (lambda (c e) (update-time-unit c e))]))
    
    (define/override  (on-size w h)
      (send slider adjust-slider-width (- w 150)))
    
    
    ))