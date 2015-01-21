#lang medic

(layer layer1
       (in #:module "fact-iter.rkt"
           [(fact) [on-entry (aggregate x a)]]))
