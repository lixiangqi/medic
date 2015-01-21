#lang medic

(layer layer1 
       (in #:module "src2.rkt"
           ; match two instances of (inc-counter)
           [(at (inc-counter)) [on-entry (log "[1]in ~a: inc-counter" @function-name)]]
           
           ; match two instances of (+ x 1)
           [(at (+ x 1) [#:before (inc-counter)]) [on-entry (log "[2]in ~a: (+ x 1)" @function-name)]]
           
           ; only match (+ x 1) in g function
           [(at (+ x 1) [#:before (define x (inc 4))
                                  _])
            [on-entry (log "[3]in ~a: (+ x 1)" @function-name)]]
           [(g) [(at (+ x 1)) [on-entry (log "[4]in ~a: (+ x 1)" @function-name)]]]
           
           ; only match (inc-counter) in function g
           [(at (inc-counter) [#:before (define x (inc 4))] [#:after (+ x 1)])
            (on-entry (log "[5]in ~a: (inc-counter)" @function-name))]
           [(at (inc-counter) [#:before (define x (inc _))] [#:after (+ x 1)])
            (on-entry (log "[6]in ~a: (inc-counter)" @function-name))]))
