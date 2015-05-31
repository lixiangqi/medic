#lang scribble/doc

@(require scribble/manual
          redex/pict
          scriblib/figure
          (for-label (except-in racket log export import remove)
                     racket/contract/base
                     medic/trace
                     (only-in medic/main 
                              layer export import define-source define-match in with-behavior each-function
                              on-entry on-exit at)))

@title[#:style '(toc)]{Medic by Example}

This section covers several small examples to help to learn the syntax of the Medic language and the use of the Medic debugging tool. 
Each example contains a source program and a medic program. Assuming that the source program, @tt{src.rkt}, is the entry 
point of the program, that the medic program is @tt{src-medic.rkt}, and that the source program, medic program, and program-starting
script are stored in the same
directory, we can start debugging by the following program-starting script:
@codeblock{
#lang racket
(require medic/core)
(medic "src-medic.rkt")
(debug "src.rkt")
}

We can also run program-starting scripts in the console window of the DrRacket programming environment. To best locate
the files, complete paths of programs---depending on the stored location---should be supplied.

@racketblock[
(require medic/core)]
@racketblock[
(medic "/home/xiangqi/medic/demos/src-medic.rkt")]
@racketblock[
(debug "/home/xiangqi/medic/demos/src.rkt")
]

The following are the demos:

@local-table-of-contents[]

@section{Demo 1: @tt{border-expr} and @tt{at-expr}}
Basic module-level and function-level insertion of some debugging code.

@bold{@tt{src1.rkt:}}
@codeblock{
#lang racket
(define z 2)
(define n 9)

(define (f x)
  (define z 5)
  (define n 4)
  (if (zero? x)
      1
      (* x (sub1 x))))

(f 3)
}

@bold{@tt{src1-medic.rkt:}}
@codeblock{
#lang medic
(layer layer1 
       (in #:module "src1.rkt"
           ; module-level border-expr
           [on-entry (define x 1)
                     (define y 2)]
           [on-exit 
            (log "module exit: expect y = 2")
            (log y)]
           ; module-level at-expr
           [at (define n 9) [on-exit (log "module at: expect n = 9")
                                     (log n)]]
           ; function-level at-expr and border-expr
           [(f) 
            [at (* x (sub1 _)) [on-entry (log "else branch: expect n = 4") (log n)]]
            [on-entry (define y 30)
                      (log "function entry: expect y = 30")
                      (log x)
                      (log y)]
            [on-exit (log "function exit: expect n = 4")
                     (log n)]])) 
}
@section{Demo 2: @tt{at-expr}}
The @racket[at-expr] pattern matching with @racket[before-expr] and @racket[after-expr] specification.

@bold{@tt{src2.rkt:}}
@codeblock{
#lang racket

(define x 10)

(define counter 0)

(define (inc-counter) (set! counter (add1 counter)))

(define (inc x) 
  (inc-counter)
  (+ x 1))

(define (g)
  (define x (inc 4))
  (inc-counter)
  (+ x 1))

(g) 
}
@bold{@tt{src2-medic.rkt:}}
@codeblock{
#lang medic
(layer layer1 
       (in #:module "src2.rkt"
           ; match two instances of (inc-counter)
           [at (inc-counter) [on-entry @"@"log{[1] in @"@"function-name : int-counter}]]
           
           ; match two instances of (+ x 1)
           [at (+ x 1) #:before (inc-counter) [on-entry @"@"log{[2]in @"@"function-name : (+ x 1)}]]
           
           ; only match (+ x 1) in g function
           [at (+ x 1) #:before (begin (define x (inc 4)) _)
               [on-entry @"@"log{[3]in @"@"function-name : (+ x 1)}]]
           [(g) [at (+ x 1) [on-entry @"@"log{[4]in @"@"function-name : (+ x 1)}]]]
           
           ; only match (inc-counter) in function g
           [at (inc-counter) #:before (define x (inc 4)) #:after (+ x 1)
               (on-entry @"@"log{[5]in @"@"function-name : (inc-counter)})]
           [at (inc-counter) #:before (define x (inc _)) #:after (+ x 1)
               (on-entry @"@"log{[6]in @"@"function-name : (inc-counter)})]))
}
@section{Demo 3: multiple functions scope}
Multiple functions involved in the debugging activity.

@bold{@tt{src3.rkt:}}
@codeblock{
#lang racket

(define counter 0)

(define (inc-counter) (set! counter (add1 counter)))

(define (inc x) 
  (inc-counter)
  (+ x 1))

(define (g x)
  (inc x))

(g 4)
}

@bold{@tt{src3-medic.rkt:}}
@codeblock{
#lang medic
(layer layer1 
       (in #:module "src3.rkt"
           ; scope of multiple functions 
           [(g inc) [on-entry @"@"log{function @"@"function-name : x = @"@"x}]]
           ; each-function primitive
           [each-function [on-entry @"@"log{function @"@"function-name entered}]]))
}

@section{Demo 4: @tt{with-behavior}}

@bold{@tt{src4.rkt:}}
@codeblock{
#lang racket

(define (f x y)
  (+ (sqr x) (sqr y)))
}
@bold{@tt{src4-medic.rkt:}}
@codeblock{
#lang medic

(layer layer1 
       (in #:module "src4.rkt"
           (with-behavior f @"@"@"{"f: sum of @"@"x squared and @"@"y squared is @"@"ret@"}")
           [on-exit (log (f 3 4))
                    (log (f 4 5))]))
}

@section{Demo 5: @tt{def}, @tt{import} and @tt{export}}
@bold{@tt{f.rkt:}}
@codeblock{
#lang racket

(provide f)

(define (f x y)
  (+ (sqr x) (sqr y)))
}
@bold{@tt{src5.rkt:}}
@codeblock{
#lang racket

(require "f.rkt")

(define t 5)

(define (g x)
  (* x (f x t)))

(g 3)
}
@bold{@tt{src5-medic.rkt:}}
@codeblock{
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
         [each-function [pos @"@"log{function @"@"function-name entered}]])
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
}