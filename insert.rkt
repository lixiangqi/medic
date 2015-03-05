(module annotator scheme/base
  
  (require (prefix-in kernel: syntax/kerncase)
           racket/list
           racket/string
           (for-syntax scheme/base)
           (only-in mzscheme [apply plain-apply])
           syntax/strip-context
           "medic-structs.rkt")
  
  (provide insert-stx)
  
  (define (arglist-bindings arglist-stx)
    (syntax-case arglist-stx ()
      [var
       (identifier? arglist-stx)
       (list arglist-stx)]
      [(var ...)
       (syntax->list arglist-stx)]
      [(var . others)
       (cons #'var (arglist-bindings #'others))]))
  
  (define (wrap-context stx bindings function-id)
    (define (lookup id)
      (findf (lambda (v)
               (equal? (syntax->datum id) (syntax->datum v)))
             bindings))
    (cond
      [(identifier? stx)
       (if (and (syntax? stx) (equal? (syntax->datum stx) '@function-name))
           (datum->syntax #f function-id)
           (let ([bound (lookup stx)])
             (if bound
                 (datum->syntax bound (syntax->datum stx) stx stx)
                 stx)))]
      [(syntax? stx)
       (wrap-context (syntax-e stx) bindings function-id)]
      [(pair? stx)
       (cons (wrap-context (car stx) bindings function-id)
             (wrap-context (cdr stx) bindings function-id))]
      [else stx]))
  
  (define (insert-stx stx insert-table at-table)
    (define top-level-ids '())
    (define let-exit '())
    (define internal-let? #f)
    (define later-removes '())
    (define (add-top-level-id var)
      (set! top-level-ids (append (list var) top-level-ids)))
    
    ; convert-stx gives library identifier the proper racket bindings
    (define (convert-stx stx)
      
      (define (attach-prop s layer-prop stamp-prop)
        (syntax-property (syntax-property s 'layer layer-prop) 'stamp stamp-prop))
      
      (define (convert-aux s)
        (map (lambda (e)
               (attach-prop (strip-context e) 
                            (syntax-property s 'layer)
                            (syntax-property s 'stamp)))
             (syntax->list s)))
      
      (define (traverse s)
        (cond
          [(syntax? s)
           (syntax-case s (log aggregate node edge timeline assert same?)
             [(log . v) (convert-aux s)]
             [(aggregate v ...) (convert-aux s)]
             [(node . v) (convert-aux s)]
             [(edge . v) (convert-aux s)]
             [(timeline v) (convert-aux s)]
             [(assert v) (convert-aux s)]
             [(same? v) (convert-aux s)]
             [else
              (traverse (syntax-e s))])]
          [(pair? s)
           (cons (traverse (car s)) (traverse (cdr s)))]
          [else s]))
      
      (define res (traverse stx))
      (if (syntax? stx)
          (datum->syntax #f res stx stx)
          (datum->syntax #f res)))
    
    ; when local? is #t, match at-pattern expression within function scope
    (define (match-at-table old-stx new-stx local? [bounds '()] [before-bounds '()] [after-bounds '()] [id #f])
      (define ret #f)
      (define before-ids (if local? (append bounds top-level-ids) before-bounds))
      (define after-ids (if local? (append bounds top-level-ids) after-bounds))
      (define to-remove-entries '())
      (define pos (syntax-position old-stx))
      (when pos
        (let iterate ([lst at-table]
                      [result-stx new-stx])
          (if (null? lst)
              (unless (equal? result-stx old-stx) (set! ret result-stx))
              (let* ([entry (first lst)]
                     [at-posns (finer-at-insert-posns entry)]
                     [insert-exprs (finer-at-insert-exprs entry)]
                     [scope (finer-at-insert-scope entry)]
                     [predicate (if local?
                                    (and (member pos at-posns)
                                         (or (member "module" scope)
                                             (member id scope)))
                                    (member pos at-posns))])
                (if predicate
                    (begin
                      (cond
                        [(or (not local?) (equal? (length scope) 1))
                         (if (equal? (length at-posns) 1)
                             (set! to-remove-entries (cons entry to-remove-entries))
                             (begin
                               (set! later-removes (cons entry later-removes))
                               (set-finer-at-insert-posns! entry (remove pos at-posns))))]
                        [(and local? (not (equal? (length scope) 1)))
                         (set-finer-at-insert-posns! entry (remove pos at-posns))
                         (set-finer-at-insert-scope! entry (remove id scope))])
                      (case (finer-at-insert-loc entry)
                        [(entry) 
                         (iterate (rest lst)
                                  (syntax-property (quasisyntax/loc old-stx (begin #,@(map (lambda (e)
                                                                                             (wrap-context (convert-stx e) before-ids id))
                                                                                           insert-exprs)
                                                                                   #,result-stx))
                                                   'debug #t))]
                        [(exit)
                         (iterate (rest lst)
                                  (syntax-property (quasisyntax/loc old-stx (begin #,result-stx
                                                                                   #,@(map (lambda (e)
                                                                                             (wrap-context (convert-stx e) after-ids id))
                                                                                           insert-exprs)))
                                                   'debug #t))]))
                    (iterate (rest lst) result-stx))))))
      (for-each (lambda (e) 
                  (set! at-table (remove e at-table)))
                to-remove-entries)
      ret)
    
    (define (match-border-insert scope loc) 
      (define inserts (hash-ref insert-table scope '()))
      (define result-stx '())
      (unless (null? inserts)
        (for-each (lambda (entry)
                    (when (equal? (insert-struct-loc entry) loc)
                      (if (equal? loc 'entry)
                          (set! result-stx (append (map convert-stx (insert-struct-exprs entry)) result-stx))
                          (set! result-stx (append result-stx (map convert-stx (insert-struct-exprs entry)))))))
                  inserts))
      result-stx)
    
    (define (top-level-insert stx)
      (kernel:kernel-syntax-case stx #f
                                 [(module identifier name mb)
                                  (module-insert stx)]
                                 [else-stx
                                  (general-top-level-expr-iterator stx)]))
 
    (define (module-insert stx)
      (syntax-case stx ()
        [(_ identifier name mb)
         (syntax-case (disarm #'mb) ()
           [(plain-module-begin . module-level-exprs)
            (with-syntax ([(module . _) stx])
              (let* ([entry-res (match-border-insert 'module 'entry)]
                     [entry-exprs (if (null? entry-res) (list #'(#%plain-app void)) entry-res)]
                     [exit-exprs (match-border-insert 'module 'exit)])
                (hash-remove! insert-table 'module)
                (cond
                  [(null? exit-exprs)
                   (quasisyntax/loc stx (module identifier name
                                       #,(rearm
                                          #'mb
                                          #`(plain-module-begin
                                             #,(datum->syntax #f '(#%require medic/trace))
                                             #,@entry-exprs
                                             #,@(map (lambda (e) (module-level-expr-iterator e))
                                                     (syntax->list #'module-level-exprs))))))]
                  [else
                   (quasisyntax/loc stx (module identifier name
                                       #,(rearm
                                          #'mb
                                          #`(plain-module-begin
                                             #,(datum->syntax #f '(#%require medic/trace))
                                             #,@entry-exprs
                                             #,@(map (lambda (e) (module-level-expr-iterator e))
                                                     (syntax->list #'module-level-exprs))
                                             #,@(map (lambda (e) (wrap-context e top-level-ids #f))
                                                  exit-exprs)))))])))])]))
    
  
    (define (module-level-expr-iterator stx)
      (kernel:kernel-syntax-case
       stx #f
       [(#%provide . provide-specs)
        stx]
       [else-stx
        (general-top-level-expr-iterator stx)]))
    
    (define (general-top-level-expr-iterator stx)
      (define before-bindings top-level-ids)
      (define new-stx 
        (kernel:kernel-syntax-case
         stx #f
         [(define-values (var ...) expr)
          (begin
            (for-each add-top-level-id (syntax->list #'(var ...)))
            (quasisyntax/loc stx
              (define-values (var ...) #,(expression-iterator #`expr '() (format "~a" (syntax->datum (car (syntax->list #'(var ...)))))))))]
         [(define-syntaxes (var ...) expr)
          stx]
         [(begin-for-syntax . exprs)
          stx]
         [(begin . top-level-exprs)
          (quasisyntax/loc stx (begin #,@(map (lambda (expr)
                                                (module-level-expr-iterator expr))
                                              (syntax->list #'top-level-exprs))))]
         [(#%require . require-specs)
          stx]
         [(module . _)
          (module-insert stx)]
         [(module* . _)
          (module-insert stx)]
         [else
          (expression-iterator stx '())]))
      (define after-bindings top-level-ids)
      (define ret (match-at-table stx new-stx #f '() before-bindings after-bindings))
      (or ret new-stx))
    
    
    (define (expression-iterator expr bound-vars [id #f])
      (define (get-lambda-exit-entry-inserts id)
        (define entry-exprs (list #'(void)))
        (define exit-exprs '())
        (when id
          (let ([entry-res (match-border-insert id 'entry)])
            (set! entry-exprs (if (null? entry-res) (list #'(void)) entry-res))
            (set! exit-exprs (match-border-insert id 'exit))
            (hash-remove! insert-table id)
            (when (hash-has-key? insert-table 'each-function)
              (set! entry-exprs (append (match-border-insert 'each-function 'entry) entry-exprs))
              (set! exit-exprs (append (match-border-insert 'each-function 'exit) exit-exprs)))))
        (values entry-exprs exit-exprs))
      
      (define (let/rec-values-annotator letrec?)
        (kernel:kernel-syntax-case
         (disarm expr) #f
         [(label (((var ...) rhs) ...) . bodies)
          (let* ([new-bindings (apply append
                                      (map syntax->list
                                           (syntax->list #`((var ...) ...))))]
                 [all-bindings (append new-bindings bound-vars)]
                 [new-rhs (map (lambda (expr)
                                 (expression-iterator expr
                                                      (if letrec? all-bindings bound-vars)
                                                      id))
                               (syntax->list #'(rhs ...)))]
                 [last-body (car (reverse (syntax->list #'bodies)))]
                 [all-but-last-body (reverse (cdr (reverse (syntax->list #'bodies))))]
                 [bodies (append (map (lambda (expr)
                                        (expression-iterator expr all-bindings id))
                                      all-but-last-body)
                                 (list (expression-iterator
                                        last-body
                                        all-bindings
                                        id)))])
            (define final-body 
              (if (or (equal? let-exit 'no-exit-exprs) (not id) (and id (regexp-match "%$" id)))
                  (with-syntax ([(new-rhs/trans ...) new-rhs])
                    (quasisyntax/loc expr
                      (label (((var ...) new-rhs/trans) ...)
                             #,@bodies)))
                  (with-syntax ([(new-rhs/trans ...) new-rhs])
                    (quasisyntax/loc expr
                      (label (((var ...) new-rhs/trans) ...)
                             #,@bodies
                             #,@(map (lambda (e) (wrap-context e (append all-bindings top-level-ids) id))
                                     let-exit))))))
            (set! let-exit 'no-exit-exprs)
            (set! internal-let? #t)
            final-body)]))
      
      (define (lambda-clause-annotator clause)
        (kernel:kernel-syntax-case
         clause #f
         [(arg-list . bodies)
          (let* ([new-bound-vars (arglist-bindings #'arg-list)]
                 [all-bound-vars (append new-bound-vars bound-vars)]
                 [bindings (append all-bound-vars top-level-ids)]
                 [body-list (syntax->list #'bodies)])
            (cond
              [(or (not id)
                   (and id (regexp-match "%$" id)))
               (define new-bodies (map (lambda (e) (expression-iterator e all-bound-vars id)) body-list))
               (quasisyntax/loc clause
                 (arg-list
                  #,@new-bodies))]
              [else
               (define-values (entry-exprs exit-exprs) (get-lambda-exit-entry-inserts id))
               (set! let-exit 'no-exit-exprs)
               (set! internal-let? #f)
               (when (and (= (length body-list) 1) (not (null? exit-exprs)))
                 (set! let-exit exit-exprs))
               (define new-bodies (map (lambda (e) (expression-iterator e all-bound-vars id)) body-list))
               (define with-entry-body (quasisyntax/loc clause
                                         (arg-list
                                          #,@(map (lambda (e) (wrap-context e bindings id))
                                                  entry-exprs)
                                          #,@new-bodies)))
               (define with-exit-body (quasisyntax/loc clause
                                        (arg-list
                                         #,@(map (lambda (e) (wrap-context e bindings id))
                                                 entry-exprs)
                                         (begin0
                                           (begin
                                             #,@new-bodies)
                                           #,@(map (lambda (e) (wrap-context e bindings id))
                                                   exit-exprs)))))
               (if (null? exit-exprs)
                   with-entry-body
                   (if internal-let?
                       with-entry-body
                       with-exit-body))]))]))
      
      (define new-stx
        (rearm
         expr
         (kernel:kernel-syntax-case
          (disarm expr) #f
          [var-stx (identifier? (syntax var-stx))
                   expr]
          
          [(#%plain-lambda . clause)
           (quasisyntax/loc expr 
             (#%plain-lambda #,@(lambda-clause-annotator #'clause)))]
          
          [(case-lambda . clauses)
           (quasisyntax/loc expr
             (case-lambda #,@(map lambda-clause-annotator (syntax->list #'clauses))))]
          
          [(if test then else)
           (quasisyntax/loc expr (if #,(expression-iterator #'test bound-vars id)
                                     #,(expression-iterator #'then bound-vars id)
                                     #,(expression-iterator #'else bound-vars id)))]
          
          [(begin . bodies)
           (quasisyntax/loc expr (begin #,@(map (lambda (e) (expression-iterator e bound-vars id)) (syntax->list #'bodies))))]
          
          [(begin0 . bodies)
           (quasisyntax/loc expr (begin0 #,@(map (lambda (e) (expression-iterator e bound-vars id)) (syntax->list #'bodies))))]
          
          [(let-values . clause)
           (let/rec-values-annotator #f)]
          
          [(letrec-values . clause) 
           (let/rec-values-annotator #t)]
          
          [(set! var val)
           (quasisyntax/loc expr (set! var #,(expression-iterator #`val bound-vars id)))]
          
          [(quote _) expr]
          
          [(quote-syntax _) expr]
          
          [(with-continuation-mark key mark body)
           (quasisyntax/loc expr (with-continuation-mark key
                                   #,(expression-iterator #'mark bound-vars id)
                                   #,(expression-iterator #'body bound-vars id)))]
          
          [(#%plain-app . exprs)
           (let ([subexprs (map (lambda (e) (expression-iterator e bound-vars id)) (syntax->list #'exprs))])
             (quasisyntax/loc expr (#%plain-app . #,subexprs)))]
          
          [(#%top . var) expr]
          [(#%variable-reference . _) expr]
            
          [else (error 'expr-syntax-object-iterator "unknown expr: ~a"
                       (syntax->datum expr))])))
      (define ret (match-at-table expr new-stx #t bound-vars '() '() id))
      (or ret new-stx))
    (begin0
      (top-level-insert stx)
      (with-handlers ([exn:fail:syntax?
                       (λ (e)
                         ((error-display-handler) 
                          (exn-message e)
                          e))])
        (for-each (lambda (e)
                    (set! at-table (remove e at-table)))
                  later-removes)
        (for-each
         (lambda (entry)
           (raise-syntax-error #f "unmatched-medic-expression" (finer-at-insert-at-expr entry)))
         at-table)
        (hash-remove! insert-table 'each-function)
        (for-each 
         (lambda (i)
           (raise-syntax-error #f "unmatched-medic-expression" (insert-struct-stx (car i))))
         (hash-values insert-table)))))
  
  (define (disarm stx) (syntax-disarm stx code-insp))
  (define (rearm old new) (syntax-rearm new old))
  (define code-insp (variable-reference->module-declaration-inspector
                     (#%variable-reference))))