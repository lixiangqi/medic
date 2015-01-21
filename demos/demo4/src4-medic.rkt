#lang medic

(layer layer1 
       (in #:module "src4.rkt"
           (with-behavior f @{f: sum of @x squared and @y squared is @ret})
           [on-exit (log (f 3 4))
                    (log (f 4 5))]))
