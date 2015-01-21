#lang racket

(require racket/gui/base)

(provide log-text%)

(define log-text%
  (class text%
    (init-field [data null])
    (inherit insert
             begin-edit-sequence
             end-edit-sequence
             get-position last-position
             change-style)
    
    (define layer-position (make-hash))
    (define layers (sort (remove-duplicates	(map second data)) string<=?))
    
    (define behavior-style (new style-delta%))
    (send behavior-style set-delta-foreground "DodgerBlue")
    
    (define highlight-style (new style-delta%))
    (send highlight-style set-delta-background "Yellow")
    (define unhighlight-style (new style-delta%))
    (send unhighlight-style set-delta-background "White")
    
    (define/private (display-logs)
      (define current-pos (box #f))
      (begin-edit-sequence)
      (for-each
       (lambda (i)
         (define to-insert (first i))
         (define layer (second i))
         (get-position current-pos)
         (define start-pos (unbox current-pos))
         (insert to-insert)
         (insert "\n")
         (get-position current-pos)
         (define end-pos (sub1 (unbox current-pos)))
         (define v (hash-ref layer-position layer '()))
         (hash-set! layer-position layer (cons (cons start-pos end-pos) v))
         (when (third i)
           (change-style behavior-style start-pos end-pos #f)))
       data)
      (end-edit-sequence))
    
    (define/private (change-layer-style layer style)
      (define ranges (hash-ref layer-position layer null))
      (begin-edit-sequence)
      (for-each (lambda (p)
                  (change-style style (car p) (cdr p) #f))
                ranges)
      (end-edit-sequence))
    
    (define/public (highlight-layer layer)
      (change-layer-style layer highlight-style))
    
    (define/public (unhighlight-layer layer)
      (change-layer-style layer unhighlight-style))
    
    (define/public (highlight-all-text) 
      (begin-edit-sequence)
      (change-style highlight-style 0 (last-position))
      (end-edit-sequence))
    
    (define/public (unhighlight-all-text) 
      (begin-edit-sequence)
      (change-style unhighlight-style 0 (last-position))
      (end-edit-sequence))
    
    (define/public (get-layers) layers)
    
    (super-new)
    (display-logs)))