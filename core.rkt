#lang racket

(require "load-annotator.rkt"
         "medic-annotator.rkt"
         "trace-browser/browser.rkt")

(provide medic
         debug)

(define medic-insert-table #f)
(define medic-at-table #f)
(define medic-template #f)

(define (append-table init to-append)
  (for ([t to-append])
    (for ([fn-key (hash-keys t)])
      (let ([val (hash-ref t fn-key)])
        (if (hash-has-key? init fn-key)
            (hash-set! init fn-key (append (hash-ref init fn-key) val))
            (hash-set! init fn-key val))))))

; fn can be relative-path or complete path
; e.g. (medic "/home/xiangqi/test/debug1.rkt"
;             "debug2.rkt")
(define (medic fn . fns)
  (let* ([files (map (lambda (f) (if (complete-path? f) `(file ,f) f)) (append (list fn) fns))]
         [tables (map (lambda (l) (dynamic-require l 'medic-table)) files)]
         [insert-tables (map first tables)]
         [insert-table (hash-copy (first insert-tables))]
         [at-tables (map second tables)]
         [at-table (hash-copy (first at-tables))]
         [templates (map third tables)]
         [template (hash-copy (first templates))])
    (for ([t (rest insert-tables)])
      (for ([fn-key (hash-keys t)])
        (cond
          [(hash-has-key? insert-table fn-key) 
           (let ([scope-table1 (hash-ref insert-table fn-key)]
                 [scope-table2 (hash-ref t fn-key)])
             (for ([scope-key (hash-keys scope-table2)])
               (let ([val2 (hash-ref scope-table2 scope-key)])
               (if (hash-has-key? scope-table1 scope-key)
                   (hash-set! scope-table1 scope-key (append (hash-ref scope-table1 scope-key) val2))
                   (hash-set! scope-table1 scope-key val2)))))]
          [else
           (hash-set! insert-table fn-key (hash-ref t fn-key))])))
    (append-table at-table (rest at-tables))
    (append-table template (rest templates))
    
    (set! medic-insert-table insert-table)
    (set! medic-at-table at-table)
    (set! medic-template template)))
   
; fn: string of complete path or relative path
(define (debug fn)
  (let ([mod (if (complete-path? fn) `(file ,fn) fn)]
        [annotate-module? (lambda (fn m)
                            (let ([fn-str (path->string fn)])
                              (or (and medic-insert-table (hash-has-key? medic-insert-table fn-str))
                                  (and medic-at-table (hash-has-key? medic-at-table fn-str)))))])
    (with-handlers ([exn:fail?
                     (λ (e) 
                       (make-trace-browser fn)
                       (raise e))])
      (eval/annotations mod annotate-module? annotate-stx medic-insert-table medic-at-table medic-template))
    (make-trace-browser fn)))
