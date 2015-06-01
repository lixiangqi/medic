#lang medic

(layer layer1
       (in #:module "se.rkt"
           [(process)
            [at (set-dragged #f)
             [on-entry (timeline (cons "left-up" dragged))]]
            
            [at (set-dragged #t)
             [on-exit (timeline (cons "left-down" dragged))]]
            
            [at (equal? event "dragging")
             [on-exit (timeline (cons "try-dragging" dragged))]]
            
            [at (set! rect-x x)
             [on-entry 
              (timeline "moving")
              (timeline rect-x)
              (timeline rect-y)]]]))