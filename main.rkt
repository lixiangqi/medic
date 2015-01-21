#lang racket
(require syntax/quote)

(require "medic-structs.rkt")

(provide (rename-out [module-begin #%module-begin])
         define log
         #%app #%top require #%datum
         #%top-interaction
         layer export import def in with-behavior ref each-function
         on-entry on-exit at)


(module reader syntax/module-reader
  medic)

(define-syntax-rule (module-begin form ...)
  (#%module-begin 
   (provide medic-table)
   (define medic-table (interp (quote-syntax/keep-srcloc (begin form ...))))))

(define (interp stx)
  
  (define counter 0)
  (define current-layer-id #f)
  (define exports '())
  (define imports '())
  (define import-table '())
  (define src-table (make-hash)) ; id -> list-of-syntax
  (define debug-table (make-hash))
  
  ;; global-env is for variables' lookup when layer importing or exporting
  ; map from layer identifier to env structure
  (define global-env (make-hash))
    
  ;; insert-table and at-inserts store the information about inserting expressions
  ;; to the source programs. 
  
  ; insert-table: map from complete path string to a hash table, which maps
  ; from scope-id to a list of pairs ((or 'exit 'entry), to-be-inserted-exprs)
  ; scope-id:
  ; - 'module
  ; - function name (string)
  ; - 'each-function
  (define insert-table (make-hash))
  
  ; at-inserts: map from complete path string to a list of at-insert structure
  (define at-inserts (make-hash))
  
  ; template: map from complete path string to a hash table, which maps
  ; function name (symbol) to (list function-template-string args return-value)
  ; one function can only take one logging behavior in one debugging program
  (define template (make-hash))
  
  (define (interpret-layer-form stx)
    (syntax-case stx (layer)
      [(layer layer-id #:enable flag layer-expr ...) (identifier? #'layer-id)
       (begin
         (set! current-layer-id (syntax->datum #'layer-id))
         (set! exports '())
         (set! imports '())
         (set! import-table '())
         (set! src-table (make-hash))
         (set! debug-table (make-hash))
         (for-each (lambda (expr) 
                     (interpret-expr expr (syntax->datum #'flag)))
                   (syntax->list #'(layer-expr ...)))
         (hash-set! global-env current-layer-id (env exports imports import-table src-table debug-table)))]    
      [(layer layer-id layer-expr ...) (identifier? #'layer-id)
       (interpret-layer-form (syntax/loc stx (layer layer-id #:enable #t layer-expr ...)))]                                          
      [else
       (raise-syntax-error #f "invalid-medic-expression" stx)]))
  
  (define (reverse-hashtable t)
    (for-each (lambda (key)
                (hash-set! t key (reverse (hash-ref t key))))
              (hash-keys t)))
  
  (define (reverse-insert-table)
    (for-each (lambda (key)
                (let ([t (hash-ref insert-table key)])
                  (reverse-hashtable t)))
              (hash-keys insert-table)))
  
  (define (process-tables)
    (reverse-insert-table)
    (reverse-hashtable at-inserts))
  
  (define (interpret-expr stx flag)
    (syntax-case stx (def in)
      [(def src-id #:src src-expr ...) (identifier? #'src-id)
       (if (member (syntax->datum #'src-id) imports)
           (raise-syntax-error 'conflicting-identifiers 
                               (format "identifier ~v already imported" (syntax->datum #'src-id))
                               stx)
           (let ([expands (flatten (map interpret-src-expr (syntax->list #'(src-expr ...))))])
             (hash-set! src-table (syntax->datum #'src-id) expands)))]
      [(def debug-id #:debug expr ...) (identifier? #'debug-id) ; expr ... is lazy evaluated, may contain unexpanded (ref ...) form
       (hash-set! debug-table (syntax->datum #'debug-id) #'(expr ...))]
      [(in #:module id expr ...)
       (if flag
           (let ([fn (path->string 
                      (resolved-module-path-name 
                       ((current-module-name-resolver) (syntax->datum #'id) #f #f)))])
             (unless (hash-has-key? insert-table fn)
               (hash-set! insert-table fn (make-hash)))
             (unless (hash-has-key? template fn)
               (hash-set! template fn (make-hash)))
             (for-each (lambda (e)
                         (interpret-match-expr e fn))
                       (syntax->list #'(expr ...))))
           (begin
             (process-tables)
             (list insert-table
                   at-inserts
                   template)))]
      [(op id ...) (equal? 'import (syntax->datum #'op))
       (let ([imported-ids (map 
                             (lambda (id) 
                               (let* ([layer-id (syntax->datum id)]
                                      [exported (env-exports (hash-ref global-env layer-id))])
                                 (set! import-table (cons (import-struct layer-id exported) import-table))
                                 exported))
                             (syntax->list #'(id ...)))])
         (set! imports (flatten imported-ids)))]
      [(op id ...) (equal? 'export (syntax->datum #'op))
       (set! exports (map syntax->datum (syntax->list #'(id ...))))] 
      [else
       (raise-syntax-error #f "invalid-medic-expression" stx)]))
  
  ; interpret-match-expr: syntax string-of-file-name -> void
  (define (interpret-match-expr stx fn)
    (define (add-template f t r)
      (let* ([fun (syntax->datum f)]
             [ts (format "~a" (syntax->datum t))]
             [str (substring ts 1 (sub1 (string-length ts)))]
             [at-exprs (remove-duplicates (regexp-match* #px"@\\s\\(.+\\)" str))]
             [at-vars (remove-duplicates (regexp-match* #px"@\\w+" str))]
             [table (hash-ref template fn)])
        (hash-set! table fun (list str (cons at-exprs at-vars) r))))
      
    (syntax-case stx (ref at with-behavior each-function renamed ret)
      [(ref debug-id)
       (let* ([id (syntax->datum #'debug-id)]
              [expr (hash-ref debug-table id #f)])
         (cond
           [expr
            (for-each (lambda (e) (interpret-match-expr e fn)) (syntax->list expr))]
           [else
            (let iterate ([lst import-table])
              (when (null? lst) 
                (raise-syntax-error 'refer-to-unbound-variable 
                                    (format "id = ~a" id)
                                    stx))
              (if (member id (import-struct-exported (car lst)))
                  (let ([found-expr (hash-ref (env-debug-table (hash-ref global-env (import-struct-layer-id (car lst)))) id)])
                     (for-each (lambda (e) (interpret-match-expr e fn)) (syntax->list found-expr)))
                  (iterate (cdr lst))))]))]
      
      [(with-behavior f @ t)
       (add-template #'f #'t #f)]
      
      [(with-behavior f @ t (renamed ret r))
       (add-template #'f #'t (format "~a" (syntax->datum #'r)))]
      
      [[each-function to-insert ...]
       (for-each (lambda (e) (interpret-insert-expr e fn (list 'each-function))) (syntax->list #'(to-insert ...)))]
      
      [[(at expr ...) border-expr ...]
       (interpret-insert-expr stx fn (list 'module))]
      
      [[(f ...) to-insert ...]
       (let ([funs (map (lambda (f) (format "~a" (syntax->datum f))) (syntax->list #'(f ...)))])
         (for-each (lambda (e) 
                     (interpret-insert-expr e fn funs stx))
                   (syntax->list #'(to-insert ...))))]
      [else
       (interpret-insert-expr stx fn (list 'module) stx)]))
  
  ; interpret-insert-expr: syntax string (list-of symbol) -> void
  (define (interpret-insert-expr stx fn scope-ids [orig-stx #f])
    (define (insert-expr loc inserts)
      (let ([table (hash-ref insert-table fn)])
        (for-each (lambda (i)
                        (let ([exist (hash-ref table i '())])
                          (hash-set! table i (cons (insert-struct orig-stx loc inserts) exist))))
                   scope-ids)))
    
    (syntax-case stx (on-entry on-exit)
      [[on-entry src-expr ...]
       (insert-expr 'entry (map interpret-src-expr (syntax->list #'(src-expr ...))))]
      [[on-exit src-expr ...]
       (insert-expr 'exit (map interpret-src-expr (syntax->list #'(src-expr ...))))]
      [else
       (interpret-at-expr stx fn (map (lambda (i) (format "~a" i)) scope-ids))]))
  
  (define (interpret-at-expr stx fn scope)
    (syntax-case stx (at)
      [[(at location-expr [#:before expr1 ...] [#:after expr2 ...]) border-expr ...]
       (let ([before-exp (syntax->list #'(expr1 ...))]
             [after-exp (syntax->list #'(expr2 ...))])
         (for-each (lambda (e) 
                     (interpret-border-expr e fn scope #'location-expr before-exp after-exp stx)) 
                   (syntax->list #'(border-expr ...))))]
      
      [[(at location-expr [#:before expr ...]) border-expr ...]
       (let ([before-exp (syntax->list #'(expr ...))])
         (for-each (lambda (e) 
                     (interpret-border-expr e fn scope #'location-expr before-exp '() stx)) 
                   (syntax->list #'(border-expr ...))))]
      
      [[(at location-expr [#:after expr ...]) border-expr ...]
       (let ([after-exp (syntax->list #'(expr ...))])
         (for-each (lambda (e) 
                     (interpret-border-expr e fn scope #'location-expr '() after-exp stx)) 
                   (syntax->list #'(border-expr ...))))]
      
      [[(at location-expr) border-expr ...]
       (for-each (lambda (e) 
                   (interpret-border-expr e fn scope #'location-expr '() '() stx)) 
                 (syntax->list #'(border-expr ...)))]
      
      [else
       (raise-syntax-error #f "invalid-medic-expression" stx)]))
  
  (define (interpret-border-expr stx fn scope target-exp [before '()] [after '()] [expr #f])
    
    (define (add-at-insert s)
      (let ([exist (hash-ref at-inserts fn '())])
        (hash-set! at-inserts fn (cons s exist))))
    
    (syntax-case stx (on-entry on-exit ref)
      [[on-entry (ref src-id)]
       (let ([found (hash-ref src-table (syntax->datum #'src-id) #f)])
         (cond
           [found 
            (interpret-border-expr (quasisyntax/loc stx [on-entry #,@found]) fn scope target-exp before after)]
           [else
            (raise-syntax-error 'refer-to-unbound-variable 
                                (format "id = ~a" (syntax->datum #'src-id))
                                stx)]))]
      
      [[on-entry src-expr ...]
       (let* ([exprs (map interpret-src-expr (syntax->list #'(src-expr ...)))]
              [at-struct (at-insert expr scope target-exp before after 'entry exprs)])
         (add-at-insert at-struct))]
      
      [[on-exit (ref src-id)]
       (let ([found (hash-ref src-table (syntax->datum #'src-id) #f)])
         (cond
           [found 
            (interpret-border-expr (quasisyntax/loc stx [on-exit #,@found]) fn scope target-exp before after)]
           [else
            (raise-syntax-error 'refer-to-unbound-variable
                                (format "id = ~a" (syntax->datum #'src-id))
                                stx)]))]
      
      [[on-exit src-expr ...]
       (let* ([exprs (map interpret-src-expr (syntax->list #'(src-expr ...)))]
              [at-struct (at-insert expr scope target-exp before after 'exit exprs)])
         (add-at-insert at-struct))]))
  
  (define (interpret-src-expr stx)
    
    (define (attach-stx-property s d)
      (set! counter (add1 counter))
      (define original-e (if (list? d) 
                             (map (lambda (i) (format "~a" (syntax->datum i))) d) 
                             (format "~a" (syntax->datum d))))
      (syntax-property (syntax-property s 'layer current-layer-id)
                       'stamp (cons counter original-e)))
      
    (syntax-case stx (ref aggregate timeline assert log same?)
      [(ref src-id)
       (let* ([id (syntax->datum #'src-id)]
              [exprs (hash-ref src-table id #f)])
         (cond
           [exprs 
            (map interpret-src-expr exprs)]
           [else
            (let iterate ([lst import-table])
              (when (null? lst)
                (raise-syntax-error 'refer-to-unbound-variable 
                                    (format "id = ~v" id)
                                    stx))
              (if (member id (import-struct-exported (car lst)))
                  (let ([found-expr (hash-ref (env-src-table (hash-ref global-env (import-struct-layer-id (car lst)))) id)])
                    (map interpret-src-expr found-expr))
                  (iterate (cdr lst))))]))]
      
      [(log v)
       (attach-stx-property stx #'v)]
      
      [(log v1 v2 ...)
       (begin
         (unless (equal? (length (regexp-match* "~a" (format "~a" (syntax->datum #'v1)))) (length (syntax->list #'(v2 ...))))
           (raise-syntax-error #f "arity mismatch" stx))
         (syntax-property (syntax-property stx 'layer current-layer-id)
                          'stamp (cons #f #f)))]
      
      [(aggregate) 
       (raise-syntax-error #f "invalid-medic-expression" stx)]
      
      [(aggregate v ...) 
       (attach-stx-property stx (syntax->list #'(v ...)))]
      
      [(same? id) (not (identifier? #'id))
       (raise-syntax-error #f "invalid-medic-expression" stx)]      
      
      [(timeline id)
       (attach-stx-property stx #'id)]
      
      [(assert cond)
       (attach-stx-property stx #'cond)]
      
      [(define (var) expr ...)
       (quasisyntax/loc stx
         (define (var)
           #,@(map interpret-src-expr (syntax->list #'(expr ...)))))]
      
      [else (syntax-property stx 'layer current-layer-id)]))
  
  (syntax-case stx (begin layer)
    [(begin (layer layer-id layer-expr ...) ...)
     (for-each (lambda (l) 
                 (interpret-layer-form l)) 
               (syntax->list #'((layer layer-id layer-expr ...) ...)))]
    [else
     (raise-syntax-error #f "invalid-medic-expression" stx)])
  (process-tables)
  (list insert-table
        at-inserts
        template))

(define layer #f)
(define export #f)
(define import #f)
(define def #f)
(define in #f)
(define with-behavior #f)
(define ref #f)
(define each-function #f)
(define on-entry #f)
(define on-exit #f)
(define at #f)