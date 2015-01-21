#lang racket

(require racket/gui
         syntax/modread
         "medic-structs.rkt"
         "insert.rkt"
         "syntax-traversal.rkt")

(provide eval/annotations)

; filename: complete-path-string
(define (build-input-port filename)
  (let ([p (open-input-file filename)])
    (port-count-lines! p)
    (let ([p (cond [(regexp-match-peek "^WXME01[0-9][0-9] ## " p)
                    (let ([t (make-object text%)])
                      (send t insert-file p 'standard)
                      (close-input-port p)
                      (open-input-text-editor t))]
                   [else p])])
      (port-count-lines! p)
      (let loop ()
        (when (regexp-match-peek "^#!" p)
          (let lloop ([prev #f])
            (let ([c (read-char-or-special p)])
              (if (or (eof-object? c)
                      (eq? c #\return)
                      (eq? c #\newline))
                  (when (eq? prev #\\)
                    (loop))
                  (lloop c))))))
      (values p filename))))

(define (eval/annotations initial-module annotate-module? annotator insert-tables at-tables templates)
  (define ns (make-base-namespace))
  (namespace-attach-module (current-namespace) 'racket/class ns)
  (parameterize
      ([current-load/use-compiled
        (let ([ocload/use-compiled (current-load/use-compiled)])
          (lambda (fn m)
            (cond [(annotate-module? fn m)
                   (let* ([fn-str (path->string fn)]
                          [insert-table (hash-ref insert-tables fn-str '())]
                          [at-table (hash-ref at-tables fn-str '())]
                          [template (hash-ref templates fn-str '())])
                     (load-module/annotate annotator fn m insert-table at-table template))]
                  [else
                   (ocload/use-compiled fn m)])))]
       [current-namespace ns])
    (eval #`(require #,initial-module))))

(define (process-at-table stx at-table)
  (define new-at-table
    (map (lambda (entry)
           (let ([positions (search-pos stx (at-insert-target entry) (at-insert-before entry) (at-insert-after entry))])
             (when (null? positions)
               (raise-syntax-error #f "unmatched-medic-expression" (at-insert-at-expr entry)))
             (finer-at-insert (at-insert-at-expr entry) (at-insert-scope entry) (at-insert-target entry) positions (at-insert-loc entry) (at-insert-exprs entry))))
         at-table))
  ; filter out empty posns in finer-at-insert structure
  (set! new-at-table (filter (lambda (a) (not (null? (finer-at-insert-posns a)))) new-at-table))
  new-at-table)

; fn: complete-path-string
(define (load-module/annotate annotator fn m insert-table at-table template)
  (let-values ([(base _ __) (split-path fn)]
               [(in-port src) (build-input-port fn)])
    (dynamic-wind
     (lambda () (void))
     
     (lambda ()
       (parameterize ([read-accept-compiled #f]
                      [current-load-relative-directory base])
         (unless m (raise 'module-name-not-passed-to-load-module/annotate))
         (with-module-reading-parameterization
          (lambda ()
            (let* ([stx (parameterize ([current-namespace (make-base-namespace)])
                          (read-syntax src in-port))]
                   [new-at-table (process-at-table stx at-table)])
              (let* ([inserted (expand (insert-stx (check-module-form (expand stx) m fn) insert-table new-at-table))]
                     [module-ized-exp (annotator (check-module-form inserted m fn) template)]
                     [second (read in-port)])
                (unless (eof-object? second)
                  (raise-syntax-error
                   'load-module/annotate
                   (format "expected only a `module' declaration for `~s', but found an extra expression" m)
                   second))
                (eval-syntax module-ized-exp)))))))
     
     (lambda () (close-input-port in-port)))))
