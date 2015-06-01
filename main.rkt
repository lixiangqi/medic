#lang racket
(require syntax/quote)

(require "medic-structs.rkt")

(provide (rename-out [module-begin #%module-begin])
         log begin
         #%app #%top require #%datum
         #%top-interaction
         layer export import define-source define-match 
         in with-behavior each-function
         on-entry on-exit at function-name)

(module reader syntax/module-reader
  medic
  #:read read
  #:read-syntax read-syntax
  (require scribble/reader))

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
  ; function name (string) to (list function-template-string args return-value)
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
    (syntax-case stx (define-source define-match in)
      [(define-source src-id src-expr) (identifier? #'src-id)
       (if (member (syntax->datum #'src-id) imports)
           (raise-syntax-error 'conflicting-identifiers 
                               (format "identifier ~v already imported" (syntax->datum #'src-id))
                               stx)
           (let ([expands (interpret-src-expr #'src-expr)])
             (hash-set! src-table (syntax->datum #'src-id) (list expands))))]
      [(define-source (src-id args ...) body ...) (identifier? #'src-id)
       (if (member (syntax->datum #'src-id) imports)
           (raise-syntax-error 'conflicting-identifiers 
                               (format "identifier ~v already imported" (syntax->datum #'src-id))
                               stx)
           (hash-set! src-table (syntax->datum #'src-id) (cons (syntax->list #'(args ...)) (syntax->list #'(body ...)))))] 
      [(define-match debug-id expr ...) (identifier? #'debug-id)  ; expr ... is lazy evaluated, may contain unexpanded (ref ...) form
       (hash-set! debug-table (syntax->datum #'debug-id) (syntax->list #'(expr ...)))]
      [(define-match (debug-id args ...) expr ...) (identifier? #'debug-id)
       (hash-set! debug-table (syntax->datum #'debug-id) (cons (syntax->list #'(args ...)) (syntax->list #'(expr ...))))]
      [(in #:module id expr ...)
       (if flag
           (let ([fn (path->string 
                      (resolved-module-path-name 
                       ((current-module-name-resolver) (syntax->datum #'id) #f #f #f)))])
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
         (set! imports (flatten imported-ids))
         (for-each (lambda (i)
                     (hash-set! src-table i 'import)
                     (hash-set! debug-table i 'import))
                   imports))]
      [(op id ...) (equal? 'export (syntax->datum #'op))
       (set! exports (map syntax->datum (syntax->list #'(id ...))))] 
      [else
       (raise-syntax-error #f "invalid-medic-expression" stx)]))
  
  ; interpret-match-expr: syntax string-of-file-name -> void
  (define (interpret-match-expr stx fn)
    (define (get-escapes strs texts)
      (define escapes null)
      (for ([i (in-range (length strs))])
        (let ([ele (list-ref strs i)])
          (unless (member ele texts)
            (set! escapes (cons (cons i (string-trim ele)) escapes)))))
      escapes)
    
    (define (add-template f t)
      (let* ([fun (format "~a" (syntax->datum f))]
             [s (format "~v" (syntax->datum t))]
             [ts (substring s 2 (sub1 (string-length s)))]
             [str-lst (string-split ts "\"")]
             [text-matches (regexp-match* #px"\"[^\"]*\"" ts)]
             [text-lst (map (lambda (x) (string-replace x "\"" "")) text-matches)]
             [escape-lst (get-escapes str-lst text-lst)]
             [table (hash-ref template fn)]
             [last (last str-lst)]
             [r #f])
        (when (regexp-match "#:renamed ret" last)
          (set! str-lst (take str-lst (sub1 (length str-lst))))
          (set! r (car (regexp-match #px"\\w+$" (string-trim last)))))
        (hash-set! table fun (list str-lst escape-lst r))))

    (define (substitute-match stx arg-ids args)
      (define (substitute-aux s)
        (cond
          [(identifier? s)
           (let loop ([i 0])
             (if (>= i (length arg-ids))
                 s
                 (if (bound-identifier=? s (list-ref arg-ids i))
                     (datum->syntax s (list-ref args i) s s)
                     (loop (add1 i)))))]
          [(syntax? s)
           (substitute-aux (syntax-e s))]
          [(pair? s)
           (cons (substitute-aux (car s)) (substitute-aux (cdr s)))]
          [else s]))
      (quasisyntax/loc stx
        #,(substitute-aux stx)))

    (define (lookup-debug-import sym fun?)
      (let iterate ([lst import-table])
        (when (null? lst) 
          (raise-syntax-error 'refer-to-unbound-variable 
                              (format "id = ~a" sym)
                              stx))
        (if (member sym (import-struct-exported (car lst)))
            (let ([found-expr (hash-ref (env-debug-table (hash-ref global-env (import-struct-layer-id (car lst)))) sym)])
              (cond
                [fun?
                 (hash-set! debug-table sym found-expr)
                 (interpret-match-expr stx fn)]
                [else
                 (for-each (lambda (e) (interpret-match-expr e fn)) found-expr)]))
            (iterate (cdr lst)))))
      
    (syntax-case stx (at with-behavior each-function renamed ret)
      [debug-id (hash-ref debug-table (syntax->datum #'debug-id) #f)
       (let* ([id (syntax->datum #'debug-id)]
              [res (hash-ref debug-table id #f)])
         (case res
           [(import) (lookup-debug-import id #f)]
           [else
            (for-each (lambda (e) (interpret-match-expr e fn)) res)]))]
      [(debug-id args ...) (hash-ref debug-table (syntax->datum #'debug-id) #f)
       (let* ([sym (syntax->datum #'debug-id)]
              [res (hash-ref debug-table sym)])
         (case res
           [(import)
            (lookup-debug-import sym #t)]
           [else
            (let* ([arg-ids (car res)]
                   [body (cdr res)]
                   [subs (map (lambda (s) (quasisyntax/loc s
                                            #,(substitute-match s arg-ids (syntax->list #'(args ...)))))
                         body)])
              (for-each (lambda (e) (interpret-match-expr e fn)) subs))]))]
      [(with-behavior f t)
       (add-template #'f #'t)]
      
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
    (define (interpret-seq s)
      (syntax-case s (begin)
        [(begin expr ...)
         (syntax->list #'(expr ...))]
        [else
         (list s)]))
    
    (syntax-case stx (at)
      [[at location-expr #:before expr1 #:after expr2 border-expr ...]
       (let ([before-exp (interpret-seq #'expr1)]
             [after-exp (interpret-seq #'expr2)])
         (for-each (lambda (e) 
                     (interpret-border-expr e fn scope #'location-expr before-exp after-exp stx)) 
                   (syntax->list #'(border-expr ...))))]
      
      [[at location-expr #:before expr border-expr ...]
       (let ([before-exp (interpret-seq #'expr)])
         (for-each (lambda (e) 
                     (interpret-border-expr e fn scope #'location-expr before-exp '() stx)) 
                   (syntax->list #'(border-expr ...))))]

      [[at location-expr #:after expr border-expr ...]
       (let ([after-exp (interpret-seq #'expr)])
         (for-each (lambda (e) 
                     (interpret-border-expr e fn scope #'location-expr '() after-exp stx)) 
                   (syntax->list #'(border-expr ...))))]

      [[at location-expr border-expr ...]
       (for-each (lambda (e) 
                   (interpret-border-expr e fn scope #'location-expr '() '() stx)) 
                 (syntax->list #'(border-expr ...)))]
      
      [else
       (raise-syntax-error #f "invalid-medic-expression" stx)]))
  
  (define (interpret-border-expr stx fn scope target-exp [before '()] [after '()] [expr #f])
    
    (define (add-at-insert s)
      (let ([exist (hash-ref at-inserts fn '())])
        (hash-set! at-inserts fn (cons s exist))))
    
    (syntax-case stx (on-entry on-exit)
      [[on-entry src-expr ...]
       (let* ([exprs (map interpret-src-expr (syntax->list #'(src-expr ...)))]
              [at-struct (at-insert expr scope target-exp before after 'entry exprs)])
         (add-at-insert at-struct))]
      
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
    
    (define (substitute-body stx arg-ids args)
      (define (substitute-aux s)
        (cond
          [(identifier? s)
           (let loop ([i 0])
             (if (>= i (length arg-ids))
                 s
                 (if (bound-identifier=? s (list-ref arg-ids i))
                     (datum->syntax s (list-ref args i) s s)
                     (loop (add1 i)))))]
          [(syntax? s)
           (substitute-aux (syntax-e s))]
          [(pair? s)
           (cons (substitute-aux (car s)) (substitute-aux (cdr s)))]
          [else s]))
      (quasisyntax/loc stx
        #,(substitute-aux stx)))
    
    (define (process-ref stx)
      (define (lookup-import sym fun?)
        (let iterate ([lst import-table])
          (when (null? lst)
            (raise-syntax-error 'refer-to-unbound-variable 
                                (format "id = ~a" sym)
                                stx))
          (if (member sym (import-struct-exported (car lst)))
              (let ([found-expr (hash-ref (env-src-table (hash-ref global-env (import-struct-layer-id (car lst)))) sym)])
                (cond
                  [fun?
                   (hash-set! src-table sym found-expr)
                   (process-ref stx)]
                  [else
                   (quasisyntax/loc stx
                     (begin
                       #,@(map interpret-src-expr found-expr)))]))
              (iterate (cdr lst)))))
        
      (syntax-case stx ()
        [id (identifier? #'id)
         (let* ([sym (syntax->datum #'id)]
                [res (hash-ref src-table sym)])
           (case res
             [(import) (lookup-import sym #f)]
             [else
              (quasisyntax/loc stx
                (begin #,@(map interpret-src-expr res)))]))]
        [(id args ...)
         (let* ([sym (syntax->datum #'id)]
                [res (hash-ref src-table sym)])
           (case res
             [(import)
              (lookup-import sym #t)]
             [else
              (let* ([arg-ids (car res)]
                     [body (cdr res)]
                     [subs-body (map (lambda (s) (substitute-body s arg-ids (syntax->list #'(args ...)))) body)])
                (quasisyntax/loc stx
                  (begin 
                    #,@(map interpret-src-expr subs-body))))]))]))
    
    (define (traverse s)
      (cond
        [(syntax? s)
         (syntax-case s (aggregate timeline assert log same?)
           [id (hash-ref src-table (syntax->datum #'id) #f)
            (process-ref s)]
      
           [(id args ...) (hash-ref src-table (syntax->datum #'id) #f)
            (process-ref s)]
           [(log v) (attach-stx-property s #'v)]
           [(log v1 v2 ...)
            (syntax-property (syntax-property s 'layer current-layer-id) 'stamp (cons #f #f))]
           [(aggregate) 
            (raise-syntax-error #f "invalid-medic-expression" s)]
           [(aggregate v ...) 
            (attach-stx-property s (syntax->list #'(v ...)))]
           [(same? id) (not (identifier? #'id))
            (raise-syntax-error #f "invalid-medic-expression" s)]
           [(timeline id)
            (attach-stx-property s #'id)]
           [(assert cond)
            (attach-stx-property s #'cond)]
           [else (traverse (syntax-e s))])]
        [(pair? s)
         (let ([fs (car s)]
               [ss (cdr s)])
           (cons (traverse fs)
                 (traverse ss)))]
        [else s]))
    
    (syntax-case stx (aggregate timeline assert log same?)
      [id (hash-ref src-table (syntax->datum #'id) #f)
       (process-ref stx)]
      
      [(id args ...) (hash-ref src-table (syntax->datum #'id) #f)
       (process-ref stx)]
      
      [(log v)
       (let ([new-stx (quasisyntax/loc stx (log #,(interpret-src-expr #'v)))])
         (attach-stx-property new-stx #'v))]
      
      [(log v1 v2 ...)
       (let* ([rest-v (map interpret-src-expr (syntax->list #'(v2 ...)))]
              [new-stx (quasisyntax/loc stx (log #,(interpret-src-expr #'v1) #,@rest-v))])
         (syntax-property (syntax-property new-stx 'layer current-layer-id) 'stamp (cons #f #f)))]
      
      [(aggregate) 
       (raise-syntax-error #f "invalid-medic-expression" stx)]
      
      [(aggregate v ...)
       (let* ([vs (map interpret-src-expr (syntax->list #'(v ...)))]
              [new-stx (quasisyntax/loc stx (aggregate #,@vs))])
         (attach-stx-property new-stx (syntax->list #'(v ...))))]
      
      [(same? id) (not (identifier? #'id))
       (raise-syntax-error #f "invalid-medic-expression" stx)]      
      
      [(timeline id)
       (let ([new-stx (quasisyntax/loc stx (timeline #,(interpret-src-expr #'id)))])
         (attach-stx-property new-stx #'id))]
      
      [(assert cond)
       (let ([new-stx (quasisyntax/loc stx (assert #,(interpret-src-expr #'cond)))])
         (attach-stx-property new-stx #'cond))]
      
      [else
       (quasisyntax/loc stx
         #,(traverse stx))]))
  
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
(define define-source #f)
(define define-match #f)
(define in #f)
(define with-behavior #f)
(define each-function #f)
(define on-entry #f)
(define on-exit #f)
(define at #f)
(define function-name #f)