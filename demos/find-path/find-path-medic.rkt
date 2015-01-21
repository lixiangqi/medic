#lang medic
 
(layer left-path
       (in #:module "find-path.rkt"
           [(at (if left-p _ _)) [on-entry (log "left branch: ~a, ~a" (cadr t) left-p)]]))
 
(layer right-path
       (in #:module "find-path.rkt"
           [(at (if right-p _ _)) [on-entry (log "right branch: ~a, ~a" (caddr t) right-p)]]))
