#lang medic

(layer layer1 #:enable #f
       (in #:module "doubly-linked-list.rkt"
           [on-exit
            (define dlist (new doubly-linked-list%))
            ; add ten elements
            (for ([i (reverse (build-list 10 values))]) (send dlist add-at 0 i))
            (for ([i (in-range (send dlist get-size))])
              (log "i=~a, datum=~a" i (send dlist element-at i)))
            
            ; remove five successive elements starting from the fourth element
            (for ([i (in-range 5)]) (send dlist remove 3))
            (for ([i (in-range (send dlist get-size))])
              (log "after removal: i=~a, datum=~a" i (send dlist element-at i)))]))

(layer layer2
       (in #:module "doubly-linked-list.rkt"
           [on-exit
            (define dlist (new doubly-linked-list%))
            (for ([i (reverse (build-list 10 values))]) (send dlist add-at 0 i))
            (for ([i (in-range 5)]) (send dlist remove 3))
            
            (for/fold ([temp (get-field head dlist)]) 
              ([i (in-range (sub1 (send dlist get-size)))])
              (define next (get-field next temp))
              (edge temp next "" "Red" (get-field datum temp) (get-field datum next))
              next)
            (for/fold ([temp (get-field next (get-field head dlist))])
              ([i (in-range (sub1 (send dlist get-size)))])
              (define prev (get-field previous temp))
              (edge temp prev "" #f (get-field datum temp) (get-field datum prev))
              (get-field next temp))]))
