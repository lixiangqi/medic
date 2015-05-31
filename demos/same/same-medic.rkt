#lang medic

(layer layer1
       (define-source (check-changed)
         (same? x)
         (same? y))
       (in #:module "same.rkt"
           [at (define y (list 1 -2 3 4 -5)) [on-exit (check-changed)]]
           [on-exit (check-changed)]))
