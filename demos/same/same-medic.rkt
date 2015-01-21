#lang medic

(layer layer1
       (def check-changed #:src (same? x)
                                (same? y))
       (in #:module "same.rkt"
           [(at (define y (list 1 -2 3 4 -5))) [on-exit (ref check-changed)]]
           [on-exit (ref check-changed)]))
