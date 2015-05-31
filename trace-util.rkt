#lang racket

(provide add-log
         add-node
         add-edge
         delete-node
         delete-edge
         record-aggregate
         record-timeline
         record-changed
         record-start-time
         get-log-data
         get-raw-graph
         get-aggregate-data
         get-timeline-data
         get-changed-data
         get-end-time
         browser-visible?
         contain-changed-data?)

(define log-data '())
(define snip-size 30)
(define raw-edges (make-hash))
(define raw-nodes '())
(define timeline-table (make-hash))
(define timeline-sequence null)
(define start-time #f)
(define end-time #f)
(define aggre-table (make-hash))
(define aggre-sequence null)
(define identifiers null)
(define changed-table (make-hash))
(define show-browser? #f)

(define (add-log str layer-id behavior?)
  (set! show-browser? #t)
  (set! log-data (cons (list str layer-id behavior?) log-data)))

(define (get-log-data) (reverse log-data))

(define (record-aggregate key labels vals)
  (define pairs (map (lambda (l v) (cons l v)) labels vals))
  (set! show-browser? #t)
  (define v (hash-ref aggre-table key '()))
  (when (null? v)
    (set! aggre-sequence (cons key aggre-sequence)))
  (hash-set! aggre-table key (cons pairs v)))

(define (add-node n node-label color)
  (set! raw-nodes (cons (list n node-label color) raw-nodes)))

(define (add-edge from to edge-label from-label to-label color)
  (define (valid-string? s)
    (and s (not (string=? (string-normalize-spaces s) "")) s))
  
  (define bi-directed? (hash-has-key? raw-edges (cons to from)))
  (cond
    [bi-directed?
     (let* ([v (hash-ref raw-edges (cons to from))]
            [e (first v)]
            [f (second v)]
            [t (third v)])
       (when (and (valid-string? e) (valid-string? edge-label))
         (set! e #f))
       (hash-set! raw-edges (cons to from) (list e f t bi-directed? (fifth v)))
       (hash-set! raw-edges (cons from to) (list edge-label from-label to-label bi-directed? color)))]
    [else
     (hash-set! raw-edges (cons from to) (list edge-label from-label to-label bi-directed? color))])
  (set! show-browser? #t))

(define (delete-node n)
  (set! raw-nodes (filter (lambda (i) (not (equal? (car i) n))) raw-nodes))
  (when (hash? raw-edges)
    (for ([edge (hash-keys raw-edges)])
      (when (or (equal? (car edge) n) (equal? (cdr edge) n))
        (hash-remove! raw-edges edge)))))

(define (delete-edge from to) 
  (when (hash? raw-edges)
    (hash-remove! raw-edges (cons from to))))

(define (check-equal? v1 v2)
  (cond
    [(and (object? v1) (object? v2))
     (define-values (c1 b1) (object-info v1))
     (define-values (c2 b2) (object-info v2))
     (define-values (name1 field-cnt1 field-name-list1 field-accessor1 field-mutator1 super-class1 skipped1?)
       (class-info c1))
     (define-values (name2 field-cnt2 field-name-list2 field-accessor2 field-mutator2 super-class2 skipped2?)
       (class-info c2))
     (if (equal? name1 name2)
         ; check equality of all public and private fields
         (let ([res1 (andmap (lambda (i) (not (false? i))) 
                             (for/list ([i (in-range field-cnt1)])
                               (check-equal? (field-accessor1 v1 i) (field-accessor2 v2 i))))])
           (if res1
               ; check equality of all public and inherited fields
               (andmap (lambda (i) (not (false? i))) 
                       (for/list ([i field-name-list1])
                         (check-equal? (dynamic-get-field i v1) (dynamic-get-field i v2))))
               #f))
         #f)]
    [else (equal? v1 v2)]))

(define (check-same? v1 v2)
  (equal?/recur (list v1) (list v2) (lambda (a b) (check-equal? a b))))

(define (convert-value d)
  (cond
    [(pair? d) 
     (cons (convert-value (car d))
           (convert-value (cdr d)))]
    [(vector? d)
     (vector-map convert-value d)]
    [(box? d)
     (convert-value (unbox d))]
    [(struct? d) (struct->vector d)]
    [(object? d) (object->vector d)]
    [(hash? d) 
     (for-each (lambda (k)
                 (hash-set! d k (convert-value (hash-ref d k))))
               (hash-keys d))
     (hash-copy d)]
    [else d]))

(define (record-changed id-stx label val cur-time)
  (set! end-time (truncate-time (- cur-time start-time)))
  (set! show-browser? #t)
  (define copy (convert-value val))
  (define label-str (format "~a" label))
  (define len (length identifiers))
  (let loop ([i 0])
    (if (< i len)
        (if (free-identifier=? (list-ref identifiers i) id-stx)
            (let* ([found (hash-ref changed-table i)]
                   [last-val (second (last found))]
                   [is-same? (check-same? copy last-val)])
              (hash-set! changed-table i (cons (list label-str copy is-same? end-time) found)))
            (loop (add1 i)))
        (begin
          (set! identifiers (append identifiers (list id-stx)))
          (hash-set! changed-table i (cons (list label-str copy #t end-time) '()))))))

(define (truncate-time val)
  (define (round-f a)
    (define ceil (exact-ceiling a))
    (define diff (- ceil a))
    (if (> diff 0.5)
        (exact-floor a)
        ceil))
  (/ (round-f (* val 100)) 100.0))

(define (record-start-time s) (set! start-time s))
(define (get-end-time) end-time)

(define (record-timeline key label value boolean? cur-time)
  (set! end-time (truncate-time (- cur-time start-time)))
  (set! show-browser? #t)
  (define v (hash-ref timeline-table key '()))
  (when (null? v)
    (set! timeline-sequence (cons key timeline-sequence)))
  (hash-set! timeline-table key (cons (list label value boolean? end-time) v)))

(define (get-raw-graph) (cons (reverse raw-nodes) raw-edges))

(define (get-timeline-data)
  (define data (for/list ([i (reverse timeline-sequence)])
                 (let* ([val (reverse (hash-ref timeline-table i))]
                        [label (first (first val))]
                        [values (map second val)]
                        [boolean? (third (first val))]
                        [time (map fourth val)])
                   (list label boolean? values time))))
  (set! timeline-sequence null)
  (set! timeline-table #f)
  data)

(define (get-aggregate-data)
  (define data (map (lambda (n) (reverse (hash-ref aggre-table n))) (reverse aggre-sequence)))
  (set! aggre-table #f)
  (set! aggre-sequence null)
  data)

(define (get-changed-data)
  (define data (for/list ([i (in-range (length identifiers))])
                 (let* ([val (reverse (hash-ref changed-table i))]
                        [label (first (first val))]
                        [values (map third val)]
                        [time (map fourth val)])
                   (list label #t values time))))
  (set! identifiers null)
  (set! changed-table #f)
  data)

(define (contain-changed-data?) (not (null? identifiers)))
(define (browser-visible?) show-browser?)