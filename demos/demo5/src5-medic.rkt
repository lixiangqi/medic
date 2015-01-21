#lang medic

(layer layer1
       (export log-function-entry)
       ; debug-src-id definition
       (def init-defs #:src (define id-count 0)
                            (define (inc-id-count) (set! id-count (add1 id-count))))
       (def inc-id-count #:src (inc-id-count))
       (def display-count #:src (log id-count))
       ; debug-id definition
       (def log-function-entry 
         #:debug 
         [each-function [on-entry (log "function ~a entered" @function-name)]])
       (in #:module "src5.rkt"
           [on-entry (ref init-defs)]
           [(at (define _ _)) [on-entry (ref inc-id-count)]]
           (ref log-function-entry)
           [on-exit (ref display-count)]))

(layer layer2
       (import layer1)
       (in #:module "f.rkt"
           (ref log-function-entry))
       (in #:module "src5.rkt"
           [on-exit (log t)]))
