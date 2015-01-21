#lang scribble/doc

@(require scribble/manual
          redex/pict
          scriblib/figure
          (for-label (except-in racket log export import remove)
                     racket/contract/base
                     medic/trace))

@title{Medic Debugger}
@author["Xiangqi Li"]

The Medic debugger is a debugging tool that incorporates a metaprogramming language to describe the
task of debugging and a full-featured tracing library to enhance the traditional debugging
technique of inserting print-like expressions into the source program. 
@local-table-of-contents[]

@include-section["language.scrbl"]

@section{Medic Tracing}

@defmodule[medic/trace]

@defproc*[([(log [datum any/c]) void?]
           [(log [form string?] [v any/c] ...) void?])]{
Adds a log entry in the Log pane. For the latter @racket[log] form, the log entry is a string @racket[form] with @litchar{~a} replaced
by the corresponding value among @racket[v]s. The number of @litchar{~a}s in @racket[form] must be
the same as the number of @racket[v]s.} Examples are as follows:
@codeblock{
(log "Hello World")
(log "function ~a entered" @"@"function-name)
}

@defproc[(node [v object?] [node-label any/c ""] [color (or/c string? #f) #f])
         void?]{
Adds a node to the Graph pane. The optional arguments @racket[node-label] and @racket[color] specify node
properties. If @racket[node] is called multiple times for the same @racket[v], only one node corresponding
to @racket[v] is shown in the Graph pane.
}
@defproc[(edge [from object?] [to object?] [edge-label any/c ""] [color (or/c string? #f) #f] [from-label any/c ""] [to-label any/c ""])
         void?]{
Generates an edge in the Graph pane connecting from @racket[from] to @racket[to].  
The optional arguments @racket[edge-label], @racket[from-label], and @racket[to-label] set the label 
properties of the edge and two connected nodes. The color of the arrow head of the edge is specified
by @racket[color]. When @racket[color] is @racket[#f], it uses the default gray color. When there 
exists no node associated with @racket[from] or @racket[to], @racket[edge] creates a new node for it
first and then adds an edge. 
}

@defproc[(aggregate [v any/c] ...) void?]{
Adds an aggregate entry in the Aggregate pane, which groups a sequence of @racket[v]s together.
}

@defproc[(timeline [v any/c]) void?]{
Adds a timeline entry in the Timeline pane, with a sequence of the value of @racket[v] ordered in time.
}

@defproc[(assert [pred boolean?]) void?]{
Adds a timeline entry in the Timeline pane, where the violations of the invariant @racket[pred] are
highlighted in red color.                                                                                  
}

@defproc[(same? [v any/c]) void?]{
Adds a timeline entry in the Timeline pane. It checks whether the value of @racket[v] changes over 
time where @racket[v] can be a primitive data type such as a number, string, or symbol or a compound data 
type including a pair, vector, hash table, structure, and class. For a compound data type, a change to 
an instance of the data type is defined as a change to any part of an instance of the data type. A change 
to an object of a class is defined to be a change to any of the object's inherited, private, or public member
fields.                                                                        
}

@section{Evaluation}

@defmodule[medic/core]

@defproc[(medic [path (or/c relative-path? complete-path?)] ...) void?]{
Evaluates the Medic programs and records debugging information. 
}

@defproc[(debug [path (or/c relative-path? complete-path?)]) void?]{
Transforms the source program located in the file path @racket[path] by installing debugging instructions
and evaluates the transformed source program. If there is any debugging traces produced by the Medic tracing
library, a graphical trace browser is opened.                                               
}

@section{Using the Medic Debugger}
Debugging with the Medic debugger involves three kinds of programs: source programs, Medic programs (by convention
ending with ``-medic.rkt''), and a program-starting
script. Medic programs represent debugging instructions about the source programs and a program-starting
script runs the Medic programs and starts debugging the source programs. After the evaluation of the program-starting
script, a debugging graphical interface is presented, which consists of four panes: a Log pane, Graph pane, 
Aggregate pane and Timeline pane.

@subsection[#:tag "log"]{Tracing Log}
Like the traditional print-like expressions, the tracing @racket[log] produces linear and textual debugging information
to identify problems in program execution. However, @racket[log] is more advanced than the traditional print-like expression in 
three ways:
@itemize[
  @item{Showing the context,}
  @item{Showing the behavior,}
  @item{Showing the layer of interest.}
  ]
The content of the log entry produced by @racket[(log datum)] varies with the datum type. If there is any context information
about the datum that is available to the debugger such as the name of @racket[datum], it is displayed along with the value 
of @racket[datum]. However, with the @racket[(with-behavior f template)] definition in the Medic program, the logging behavior
of @racket[f] function calls is switched to displaying the behavior of the function @racket[f].

Suppose the value of @racket[x] is 3 and we call @racket[(log x)]. Instead of merely printing out the value of @racket[x],
it prints out ``@racket[x] = 3'', which displays the extra context information of the value 3---it is the variable
@racket[x] that we are inspecting. 

All traditional print-like expressions are concerned with displaying values of data, but under some debugging 
circumstances, showing the behavior of data is needed. Consider the following example:

@racketblock[
(define (f x y)
  (+ (sqr x) (sqr y)))
]

When we call @racket[(log (f 3 4))], it produces a tracing log ``(@racket[f] 3 4) = 25'', which reveals no information
about what the @racket[f] function does. To change the behavior of @racket[(log (f 3 4))], we can modify
the Medic program by adding 
@codeblock{
(with-behavior f @"@"@"{"f: @"@"x squared plus @"@"y squared is @"@"ret@"}")
}
Then the call of @racket[(log (f 3 4))] generates ``f: 3 squared plus 4 squared is 25''. Programmers have control
over writing functions descriptions to obtain desired logging behaviors of function calls. The @racket[with-behavior] forms allows
programmers to change all logging behaviors of the same function call at different places just by changing the description at
one place.

In general, traces become harder to understand with an increase of size and create a need for programmers to be able to focus on
only the interesting parts of the trace. The @emph{layer Viewer} feature of @racket[log] offers a way to focus on relevant traces while
preserving the execution order of traces. 

The following source program traverses a tree to find the path to a desired node. 
@codeblock{
#lang racket

(define (find-path t name)
  (cond
    [(string? t) (if (equal? t name) '() #f)]
    [else
     (let ([left-p (find-path (cadr t) name)])
       (if left-p 
           (cons (car t) left-p)
           (let ([right-p (find-path (caddr t) name)])
             (if right-p 
                 (cons (car t) right-p)
                 #f))))]))

(find-path '("a" ("b" "1" "2") ("c" "3" "4")) "3")
}
Suppose we want to insert some @racket[log] expressions to see how the tree is traversed.
@codeblock{
#lang medic

(layer left-path
       (in #:module "find-path.rkt"
           [(at (if left-p _ _)) [on-entry (log "left branch: ~a, ~a" (cadr t) left-p)]]))
 
(layer right-path
       (in #:module "find-path.rkt"
           [(at (if right-p _ _)) [on-entry (log "right branch: ~a, ~a" (caddr t) right-p)]]))
}
We start a debugging session, and a trace browser is opened after the evaluation of Medic programs and augmented source programs.
@centered{@image{scribblings/layer1.png}}
What if we just want to see the path of left branches? By clicking on the ``Log Viewer'' button, a Layer Viewer window pops up, 
displaying check boxes of existing layer names. 
@centered{@image{scribblings/layer2.png}}
Select the @racket[left-path] check box, the Log Viewer is updated immediately, highlighting the traces which belong to the 
layer @racket[left-path].
@centered{@image{scribblings/layer3.png}}
@subsection{Tracing Graph}
A tracing graph presents a new means of tracing, allowing programmers to visually see the @emph{spatial} relationship
between trace elements. Text-based and linear traces can print out primitive values and preserve the execution order of 
programs but are limited for values that are reference types or compound data structures, and may exhibit connective relationship. The tracing
graph eases the burden of programmers visualizing the @emph{spatial} relationship mentally or drawing the graph manually on the paper by
adding a lot of text-based tracing functions to print out the relationship, which is fundamentally @emph{textual} and not @emph{visual}. To avoid any overlap of graph nodes and
achieve an aesthetically pleasing visual effect, the tracing graph uses force-directed algorithms for layout.

Here is one example illustrating the effectiveness of tracing graphs to find a bug in programs that is hard to uncover in
text-based traces.

Suppose we have a correct implementation of a doubly linked list with support for common accessing, inserting, and 
removing operations. We comment out line 96 to create a bug. 

@codeblock[#:line-numbers 1]{
#lang racket

(define node%
  (class object%
    (super-new)
    (init-field [datum 0])
    (field [next #f]
           [previous #f])))

(define doubly-linked-list%
  (class object%
    (field [head #f]
           [tail #f])
    (super-new)
    (define size 0)
    
    (define/public (initialize d)
      (set! head (new node% [datum d]))
      (set! tail head)
      (set! size 1))
    
    (define/public (element-at i)
      (when (or (> i (sub1 size)) (< i 0))
        (error 'element-at-invalid-argument))
      (define temp head)
      (let loop ()
        (when (not (zero? i))
          (set! temp (get-field next temp))
          (set! i (sub1 i))
          (loop)))
      (get-field datum temp))
    
    (define/public (get-size) size)
    
    (define/public (add d)
      (cond
        [(zero? size) (initialize d)]
        [else
         (define temp (new node% [datum d]))
         (set-field! previous temp tail)
         (set-field! next tail temp)
         (set! tail temp)
         (set! size (add1 size))]))
    
    (define/public (add-at i d)
      (when (or (< i 0) (> i size))
        (error 'add-invalid-arguments))
      (if (= i size)
          (add d)
          (cond
            [(zero? i)
             (define temp (new node% [datum d]))
             (set-field! next temp head)
             (set-field! previous head temp)
             (set! head temp)
             (set! size (add1 size))]
            [else
             (define temp (new node% [datum d]))
             (define p head)
             (for ([j (in-range i)])
               (set! p (get-field next p)))
             (set-field! next temp p)
             (define p-prev (get-field previous p))
             (set-field! previous temp p-prev)
             (set-field! next p-prev temp)
             (set-field! previous p temp)
             (set! size (add1 size))])))
    
    (define/public (remove i)
      (when (or (< i 0) (> i (sub1 size)))
        (error 'remove-invalid-argument))
      (cond
        [(zero? i)
         (define res (get-field datum head))
         (set! head (get-field next head))
         (if head
             (set-field! previous head #f)
             (set! tail #f))
         (set! size (sub1 size))
         res]
        [else
         (cond
           [(= i (sub1 size))
            (define res (get-field datum tail))
            (set! tail (get-field previous tail))
            (set-field! next tail #f)
            (set! size (sub1 size))
            res]
           [else
            (define temp head)
            (for ([j (in-range i)]) (set! temp (get-field next temp)))
            (define res (get-field datum temp))
            (define temp-prev (get-field previous temp))
            (define temp-next (get-field next temp))
            (set-field! next temp-prev temp-next)
            ;(set-field! previous temp-next temp-prev)
            (set! size (sub1 size))
            res])]))))
}

Now we want to verify the correctness of the implementation. Traditionally, we would write the following Medic program to debug the source 
implementation. 
@codeblock{
#lang medic

(layer layer1 
       (in #:module "doubly-linked-list.rkt"
           [on-exit
            (define dlist (new doubly-linked-list%))
            ; add ten elements
            (for ([i (reverse (build-list 10 values))]) (send dlist add-at 0 i))
            (for ([i (in-range (send dlist get-size))])
              (log "i=~a, datum=~a" i (send dlist element-at i)))
            
            ; remove five successive elements starting from the fourth element
            (for ([i (in-range 5)]) (send dlist remove 3))
            (for ([i (in-range (send dlist get-size))])
              (log "after removal: i=~a, datum=~a" i (send dlist element-at i)))]))
}

We are presented with a trace browser window containing a Log pane:
@centered{@image{scribblings/log.png}}
It seems like the insertion operation with the list behaves correctly, but there is something wrong with the removal operation---the final list should be the sequence 0, 1, 2, 8, 9 
instead of 0, 1, 2, 4, 5. The tracing logs give us little insight into the cause of the problem, and setting a breakpoint
to step though the program and examine the @racket[previous] and @racket[next] references of each node 
requires a substantial amount of time. If we modify the Medic program by trying the tracing graph, we can see
the problem instantly.
@codeblock{
#lang medic
;; disable this layer first
(layer layer1 #:enable #f
       (in #:module "doubly-linked-list.rkt"
           [on-exit
            (define dlist (new doubly-linked-list%))
            ; add ten elements
            (for ([i (reverse (build-list 10 values))]) (send dlist add-at 0 i))
            (for ([i (in-range (send dlist get-size))])
              (log "i=~a, datum=~a" i (send dlist element-at i)))
            
            ; remove five successive elements starting from the fourth element
            (for ([i (in-range 5)]) (send dlist remove 3))
            (for ([i (in-range (send dlist get-size))])
              (log "after removal: i=~a, datum=~a" i (send dlist element-at i)))]))
              
;; add a new layer using graph visualization
(layer layer2
       (in #:module "doubly-linked-list.rkt"
           [on-exit
            (define dlist (new doubly-linked-list%))
            (for ([i (reverse (build-list 10 values))]) (send dlist add-at 0 i))
            (for ([i (in-range 5)]) (send dlist remove 3))
            (for/fold ([temp (get-field head dlist)]) 
              ([i (in-range (sub1 (send dlist get-size)))])
              (define next (get-field next temp))
              ; draw an edge from the current node to its next referencing node with the red arrow color
              (edge temp next "" "Red" (get-field datum temp) (get-field datum next))
              next)
            (for/fold ([temp (get-field next (get-field head dlist))])
              ([i (in-range (sub1 (send dlist get-size)))])
              (define prev (get-field previous temp))
              ; draw an edge from the current node to its previous referencing node with the default gray arrow color
              (edge temp prev "" #f (get-field datum temp) (get-field datum prev))
              (get-field next temp))]))
}
We restart the debugging session and the trace browser is opened where the edges and nodes are visualized in the 
Graph pane. From the graph, we can visually notice that the doubly linked list is broken: a correct list should
have the property that every edge between nodes is bi-directed. After the first @racket[remove] operation, the previous
reference of node 4 is still pointing to the old node 3. Through the graph visualization, we can narrow the problem 
to an incorrect reference update using the @racket[remove] method, and the information leads us to go back to the relevant
code in the @racket[remove] implementation and find the bug. 

@centered{@image{scribblings/graph.png}}
@subsection{Aggregate View}
The aggregate view tackles the problem of grouping multiple trace elements that may be spatially separated in the source
program or are relevant to each other, for example in the same control flow of the program or belonging to the same category of
features. Also it supports @emph{scrubbing} and data comparisons @emph{diff}.

To illustrate, the source program and Medic program are as follows.
@codeblock{
#lang racket

(define (fact x a)
  (if (zero? x)
      a
      (fact (sub1 x) (* x a))))

(fact 3 1)
}
@codeblock{
#lang medic

(layer layer1
       (in #:module "fact-iter.rkt"
           [(fact) [on-entry (aggregate x a)]]))
}

The Aggregate pane of the trace browser displays the following aggregate view of data, where the values of @racket[x]
and @racket[a] are grouped together in each column. 
@centered{@image{scribblings/aggre.png}}

If traces grow overwhelming, we can click on the light-red circle button to the left of the entries and open a scrub view window. The 
scrub view allows us to focus on values at one step by scrubbing through the traces recorded in the temporal dimension.

@centered{@image{scribblings/scrub1.png}}

For data comparisons, we can right-click on either of the two slider handles---turning red when right-clicked---to mark the step 
of data to which we want to compare the current values. Differences between two steps of data are highlighted in pink.

@centered{@image{scribblings/scrub2.png}}
@subsection{Timeline View}

The timeline view focuses on showing the panorama of individual trace elements. It allows programmers to get an overview of the pattern of
changes of values over time at a glance and examine values of interest. The vertical axis of the Timeline pane records
each trace element and the horizontal axis represents values of each trace element over time. There is a timeline slider on 
the top of the Timeline pane. The timeline slider can step through the timeline showing multiple values at the same
horizontal coordinates at the same time. Clicking on any timeline square unit pops up a tooltip window showing current
individual value. 

@itemize[
  @item{@racket[(timeline v)]
         
         If the data types of @racket[v] over time are all @emph{numbers}, a line plot is rendered on the timeline. 
         For @emph{boolean} values,
         the timeline is composed of colored square units, red denoting false values and blue denoting true values. For other mixed
         data types, the literal value is displayed.
         
         One example:
@codeblock{
#lang racket

(define (count-length v count)
  (if (null? v)
      count
      (count-length (cdr v) (+ count 1))))

(count-length (cons 8 (cons 9 '())) 0)
} 
@codeblock{
#lang medic

(layer layer1
       (in #:module "count.rkt"
           [(count-length) [on-entry (timeline count)
                                     (timeline v)
                                     (timeline (null? v))]]))
} 
 The timeline:
@centered{@image{scribblings/timeline.png}}
 The timeline slider:
@centered{@image{scribblings/slider.png}} 
         }
  @item{@racket[(assert pred)]
         
         Asserts @racket[pred] to be true. If it fails, the square unit corresponding to the failed value is highlighted in red. 
         Other square units remain the default gray background color.
         
         One example:
@codeblock{
#lang racket

(define (fact x)
  (if (zero? x)
      1
      (* x (fact (- x 1)))))

(fact 3)
}
@codeblock{
#lang medic

(layer layer1
       (in #:module "fact.rkt"
           [(fact) [on-entry (assert (> x 0))]]))
}
The timeline:
@centered{@image{scribblings/assert.png}} 
         }
  @item{@racket[(same? v)]
         
         Traditional debuggers are usually only concerned with primitive data values. We define the 
         state of a compund data element as a permutation of possible states of the primitive data members. If programmers want to 
         know whether the state of a compound data changes or not, they need to memorize the previous histories of primitive 
         data members, examine all the current primitive data members at a micro level and make comparisons, which requires 
         a significant amount of debugging effort. The Medic debugger saves the debugging work that programmers have to do 
         mentally and manually by treating the compound data of interest as a @emph{unit}, the same as the 
         primitive data in the source program, and monitoring value changes at a macro level.          
         
         When @racket[(same? v)] is called for the first time, it always returns true. Aside from the first call, if the 
         current state of @racket[v] is different from the previous state, the call returns false; otherwise, the value 
         is true. Like @racket[(assert pred)], only false values are highlighted in red in the timeline.
         
         Here is one debugging example.
         
@codeblock{
#lang racket

(define x (vector 1 2 3 4 5))
(define y (list 1 -2 3 4 -5))

(define (convert-to-vector v)
  (cond
    [(vector? v) v]
    [(list? v) (apply vector v)]))

(define (make-new-vector v)
  (vector-set! (convert-to-vector v) 0 #f))

(make-new-vector x)
(make-new-vector y)        
}  
         The source program intends to create a new vector based on the original array structures without
         modifying them. In order to verify that the old array structures remain unchanged, a Medic program
         with @racket[same?] is added. 
@codeblock{
#lang medic

(layer layer1
       (def check-changed #:src (same? x)
                                (same? y))
       (in #:module "same.rkt"
           [(at (define y (list 1 -2 3 4 -5))) [on-exit (ref check-changed)]]
           [on-exit (ref check-changed)]))       
}
In the Timeline pane, we can clearly see that the source program is erroneous, because the value of @racket[x] is changed. 

@centered{@image{scribblings/same.png}}
         }
]
@include-section["demo.scrbl"]