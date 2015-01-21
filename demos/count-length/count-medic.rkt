#lang medic

(layer layer1
       (in #:module "count.rkt"
           [(count-length) [on-entry (timeline count)
                                     (timeline v)
                                     (timeline (null? v))]]))
