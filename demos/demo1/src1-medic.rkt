#lang medic

; test whether variables in debugging program have the right binding
(layer layer1 
       (in #:module "src1.rkt"
           ; module-level border-expr
           [on-entry (define x 1)
                     (define y 2)]
           [on-exit 
            (log "module exit: expect y = 2")
            (log y)]
           ; module-level at-expr
           [(at (define n 9)) [on-exit (log "module at: expect n = 9")
                                       (log n)]]
           ; function-level at-expr and border-expr
           [(f) 
            [(at (* x (sub1 _))) [on-entry (log "else branch: expect n = 4") (log n)]]
            [on-entry (define y 30)
                      (log "function entry: expect y = 30")
                      (log x)
                      (log y)]
            [on-exit (log "function exit: expect n = 4")
                     (log n)]])) 
