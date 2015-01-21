#lang medic

(layer layer1
       (in #:module "fact.rkt"
           [(fact) [on-entry (assert (> x 0))]]))
