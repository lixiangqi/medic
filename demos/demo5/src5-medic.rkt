#lang medic

(layer layer1
       (export log-function-entry)
       ; debug-src-id definition
       (define-source (init-defs) 
         (define id-count 0)
         (define (inc-id-count) (set! id-count (add1 id-count))))
       (define-source (display-count) (log id-count))
       ; debug-id definition
       (define-match (log-function-entry pos) 
         [each-function [pos @log{function @function-name entered}]])
       (in #:module "src5.rkt"
           [on-entry (init-defs)]
           [at (define _ _) [on-entry (inc-id-count)]]
           (log-function-entry on-entry)
           [on-exit (display-count)]))

(layer layer2
       (import layer1)
       (in #:module "f.rkt"
           (log-function-entry on-exit))
       (in #:module "src5.rkt"
           [on-exit (log t)]))
