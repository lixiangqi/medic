(module annotator scheme/base
  
  (require (prefix-in kernel: syntax/kerncase)
           (for-syntax scheme/base)
           (only-in mzscheme [apply plain-apply])
           racket/string
           "trace-util.rkt"
           "trace.rkt")
  (provide annotate-stx)
  
  (define (arglist-bindings arglist-stx)
    (syntax-case arglist-stx ()
      [var
       (identifier? arglist-stx)
       (list arglist-stx)]
      [(var ...)
       (syntax->list arglist-stx)]
      [(var . others)
       (cons #'var (arglist-bindings #'others))]))
  
  (define (annotate-stx stx template)
    (define top-level-ids '())
    (define args-table (make-hash))
    
    (define (add-top-level-id var)
      (set! top-level-ids (cons var top-level-ids)))
    
    (define (top-level-annotate stx)
      (kernel:kernel-syntax-case stx #f
                                 [(module identifier name mb)
                                  (module-annotate stx)]
                                 [else-stx
                                  (general-top-level-expr-iterator stx)]))
    
    (define (module-annotate stx)
      (syntax-case stx ()
        [(_ identifier name mb)
         (syntax-case (disarm #'mb) ()
           [(plain-module-begin . module-level-exprs)
            (with-syntax ([(module . _) stx])
              (quasisyntax/loc stx (module identifier name
                                     #,(rearm
                                        #'mb
                                        #`(plain-module-begin
                                           (#%require racket/base
                                                      mzlib/string)
                                           (define old (current-inspector))
                                           (current-inspector (make-inspector old))
                                           #,@(map (lambda (e) (module-level-expr-iterator e))
                                                   (syntax->list #'module-level-exprs)))))))])]))
    
    (define (module-level-expr-iterator stx)
      (kernel:kernel-syntax-case
       stx #f
       [(#%provide . provide-specs)
        stx]
       [else-stx
        (general-top-level-expr-iterator stx)]))
    
    (define (general-top-level-expr-iterator stx)
      
      (kernel:kernel-syntax-case
       stx #f
       [(define-values (var ...) expr)
        (begin
          (for-each add-top-level-id (syntax->list #'(var ...)))
          (quasisyntax/loc stx
            (define-values (var ...) #,(annotate #`expr (syntax->datum (car (syntax->list #'(var ...))))))))]
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
        (module-annotate stx)]
       [(module* . _)
        (module-annotate stx)]
       [else
        (annotate stx)]))
    
    (define (annotate expr [id #f])
      
      (define (let/rec-values-annotator letrec?)
        (kernel:kernel-syntax-case
         (disarm expr) #f
         [(label (((var ...) rhs) ...) . bodies)
          (let ([new-rhs (map (lambda (e) (annotate e id)) (syntax->list #'(rhs ...)))]
                [bodies (map (lambda (e) (annotate e id)) (syntax->list #'bodies))])
            (with-syntax ([(new-rhs/trans ...) new-rhs])
              (quasisyntax/loc expr
                (label (((var ...) new-rhs/trans) ...)
                       #,@bodies))))]))
      
      (define (lambda-clause-annotator clause)
        (kernel:kernel-syntax-case
         clause #f
         [(arg-list . bodies)
          (let* ([new-bound-vars (arglist-bindings #'arg-list)]
                 [arg-strs (map (lambda (v) (format "~a" (syntax->datum v))) new-bound-vars)]
                 [new-bodies (map (lambda (e) (annotate e id)) (syntax->list #'bodies))])
            (when (hash-has-key? template id)
              (hash-set! args-table id arg-strs))
            (quasisyntax/loc clause
                  (arg-list
                   #,@new-bodies)))]))
     
      (define (log-expression-annotator e label layer-id)
        (define (substitute-val str from to)
          (if (null? from)
              str
              (substitute-val (string-replace str (car from) (car to)) (cdr from) (cdr to))))
        
        (define (fill-val str vals)
          (if (null? vals)
              str
              (fill-val (string-replace str "~a" (car vals) #:all? #f) (cdr vals))))
                 
        (syntax-case e ()
          [(id) (identifier? #'id)
           (quasisyntax/loc e (#,add-log (format "~a = ~v" 'id id) '#,layer-id #f))] 
          [(app) (equal? (syntax->datum (car (syntax->list #'app))) '#%app)
           (let* ([app-lst (syntax->list #'app)]
                  [fun (cadr app-lst)]
                  [args (cddr app-lst)]
                  [fun-name (syntax->datum fun)]
                  [fun-args (hash-ref args-table fun-name null)]
                  [v (hash-ref template fun-name #f)])
             (cond
               [(identifier? fun)
                (if v
                    (let* ([template-str (car v)]
                           [template-at-exprs (car (cadr v))]
                           [template-at-vars (cdr (cadr v))]
                           [ret (caddr v)]
                           [template-ret (if ret ret "ret")])
                      (quasisyntax/loc e
                        (let* ([arg-values (list #,@args)]
                               [ret-value (format "~v" (apply #,fun arg-values))]
                               [at-expr-vals null]
                               [at-var-vals null]
                               [str #,template-str])
                          (parameterize ([current-namespace (make-base-namespace)])
                            (for-each (lambda (a v)
                                        (namespace-set-variable-value! (string->symbol a) v))
                                      (list #,@fun-args) arg-values)
                            (namespace-set-variable-value! (string->symbol #,template-ret) ret-value)
                            (set! at-expr-vals (map (lambda (e)
                                                      (format "~a" (eval-string (string-trim e "@ "))))
                                                    (list #,@template-at-exprs)))
                            (set! at-var-vals (map (lambda (v)
                                                     (format "~a" (eval-string (string-trim v "@"))))
                                                   (list #,@template-at-vars))))
                          (set! str (#,substitute-val str (list #,@template-at-exprs) at-expr-vals))
                          (set! str (#,substitute-val str (list #,@template-at-vars) at-var-vals))
                          (#,add-log str '#,layer-id #t))))
                    (quasisyntax/loc e (#,add-log (format "~a = ~v" #,label app) '#,layer-id #f)))]
               [else
                (error 'log-expression-annotator "unknown expr: ~a"
                       (syntax->datum e))]))]
          [(other-exp)
           (quasisyntax/loc e (#,add-log (format "~v" other-exp) '#,layer-id #f))]
          [(v1 v2 ...)
           (quasisyntax/loc e
             (#,add-log (#,fill-val v1 (map (lambda (i) (format "~a" i)) (list v2 ...))) '#,layer-id #f))]))
      
      (define (node-expression-annotator e)
        (syntax-case e ()
          [(n)
           (quasisyntax/loc e
             (#%plain-app #,add-node n "" #f))]
          [(n node-label)
           (quasisyntax/loc e
             (#%plain-app #,add-node n (format "~a" node-label) #f))]
          [(n node-label color)
           (quasisyntax/loc e
             (#%plain-app #,add-node n (format "~a" node-label) color))]))
      
      (define (edge-expression-annotator e)
        (syntax-case e ()
          [(from to)
           (quasisyntax/loc e
             (#%plain-app #,add-edge from to "" "" "" #f))]
          [(from to edge-label)
           (quasisyntax/loc e
             (#%plain-app #,add-edge from to (format "~a" edge-label) "" "" #f))]
          [(from to edge-label color)
           (quasisyntax/loc e
             (#%plain-app #,add-edge from to 
                          (format "~a" edge-label)
                          "" ""
                          color))]
          [(from to edge-label color from-label)
           (quasisyntax/loc e
             (#%plain-app #,add-edge from to 
                          (format "~a" edge-label)
                          (format "~a" from-label) ""
                          color))]
          [(from to edge-label color from-label to-label)
           (quasisyntax/loc e
             (#%plain-app #,add-edge from to 
                          (format "~a" edge-label)
                          (format "~a" from-label) (format "~a" to-label)
                          color))]))
      
      (define (get-syntax-property e key)
        (or (syntax-property e key)
            (syntax-property (cadr (syntax->list e)) key)))
          
      (define annotated
        (rearm
         expr
         (kernel:kernel-syntax-case*
          (disarm expr) #f (log aggregate node edge timeline assert same?)
          [var-stx (identifier? (syntax var-stx)) 
           expr]
          
          [(#%plain-lambda . clause)
           (quasisyntax/loc expr 
             (#%plain-lambda #,@(lambda-clause-annotator #'clause)))]
          
          [(case-lambda . clauses)
           (quasisyntax/loc expr
             (case-lambda #,@(map lambda-clause-annotator (syntax->list #'clauses))))]
          
          [(if test then else)
           (quasisyntax/loc expr (if #,(annotate #'test id)
                                     #,(annotate #'then id)
                                     #,(annotate #'else id)))]
          
          [(begin . bodies)
           (quasisyntax/loc expr (begin #,@(map (lambda (e) (annotate e id)) (syntax->list #'bodies))))]
          
          [(begin0 . bodies)
           (quasisyntax/loc expr (begin0 #,@(map (lambda (e) (annotate e id)) (syntax->list #'bodies))))]
          
          [(let-values . clause)
           (let/rec-values-annotator #f)]
          
          [(letrec-values . clause) 
           (let/rec-values-annotator #t)]
          
          [(set! var val)
           (quasisyntax/loc expr (set! var #,(annotate #`val id)))]
         
          [(quote _) expr]
          
          [(quote-syntax _) expr]
          
          [(with-continuation-mark key mark body)
           (quasisyntax/loc expr (with-continuation-mark key
                                   #,(annotate #'mark id)
                                   #,(annotate #'body id)))]
          
          [(#%plain-app log . data)
           (let ([label (cdr (get-syntax-property expr 'stamp))])
             (log-expression-annotator #'data label (format "~a" (get-syntax-property expr 'layer))))]
          
          [(#%plain-app aggregate v ...)
           (let* ([stamp (get-syntax-property expr 'stamp)]
                  [id (car stamp)]
                  [labels (cdr stamp)])
             (quasisyntax/loc expr
               (#,record-aggregate #,id (list #,@labels) (list v ...))))]
          
          [(#%plain-app node . args)
           (node-expression-annotator #'args)]
          
          [(#%plain-app edge . args)
           (edge-expression-annotator #'args)]
          
          [(#%plain-app timeline id)
           (let* ([stamp (get-syntax-property expr 'stamp)]
                  [timeline-id (car stamp)]
                  [label (cdr stamp)])
             (quasisyntax/loc expr
               (#%plain-app #,record-timeline #,timeline-id #,label id #f)))]
          
          [(#%plain-app assert cond)
           (let* ([stamp-id (get-syntax-property expr 'stamp)]
                  [id (car stamp-id)]
                  [label (cdr stamp-id)])
             (quasisyntax/loc expr
               (#%plain-app #,record-timeline #,id #,label cond #t)))]
          
          [(#%plain-app same? id)
           (quasisyntax/loc expr
              (parameterize ([current-inspector old])
                (#%plain-app #,record-changed #'id 'id id)))]
                        
          [(#%plain-app . exprs)
           (let ([subexprs (map (lambda (expr) 
                                  (annotate expr id))
                                (syntax->list #'exprs))])
             (quasisyntax/loc expr
               (#%plain-app . #,subexprs)))]
          
          [(#%top . var) expr]
          [(#%variable-reference . _) expr]
          
          [else (error 'expr-syntax-object-iterator "unknown expr: ~a"
                       (syntax->datum expr))])))
      annotated)
    
    (top-level-annotate stx))
  
  (define (disarm stx) (syntax-disarm stx code-insp))
  (define (rearm old new) (syntax-rearm new old))
  (define code-insp (variable-reference->module-declaration-inspector
                     (#%variable-reference))))
