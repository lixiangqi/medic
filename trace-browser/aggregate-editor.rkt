#lang racket

(require racket/gui/base
         (for-syntax images/icons/style pict)
         images/compile-time
         "scrub-slider.rkt")

(provide aggregate-editor%)

(define aggregate-snip%
  (class editor-snip%
    (inherit get-extent)
    
    (super-new)
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define bh (box 0))
      (define rect-width 3)
      (send dc set-pen (send the-pen-list find-or-create-pen "LightGray" 0 'solid))
      (send dc set-brush (send the-brush-list find-or-create-brush "LightGray" 'solid))
      (get-extent dc x y #f bh)
      (send dc draw-rectangle x y rect-width (unbox bh))
      (super draw dc x y left top right bottom dx dy draw-caret))))

(define scrub-icon
  (compiled-bitmap 
   (bitmap-render-icon 
    (pict->bitmap (colorize (filled-ellipse 20 20) "tomato")) 0 glass-icon-material)))

(define (data-list->string l)
  (define items (map (lambda (p) (format "~a = ~v" (car p) (cdr p))) l))
  (apply string-append (add-between items "\n")))

(define scrub-snip%
  (class image-snip%
    (field [series null]
           [frame-width 400]
           [frame-height 400])
    
    (define scrub-frame #f)
    (define main-panel #f)
    (define scrub-text #f)
    (define scrub-slider #f)
    
    (define changed-style (new style-delta%))
    (send changed-style set-delta-background "MistyRose")
    
    (define scrub-text%
      (class text%
        (inherit insert
                 delete
                 lock
                 last-position
                 change-style
                 begin-edit-sequence
                 end-edit-sequence)
        (super-new)
        
        (define/public (initialize)
          (define v (list-ref series 0))
          (lock #f)
          (begin-edit-sequence)
          (insert (data-list->string v))
          (end-edit-sequence)
          (lock #t))
        
        (define/public (display-current-trace i compare-point)
          (define v (list-ref series i))
          (define background-color #f)
          (define to-insert null)
          (lock #f)
          (begin-edit-sequence)
          (delete 0 (last-position))
          (cond
            [compare-point
             (define compare-v (list-ref series compare-point))
             (for-each (lambda (compare-e e)
                         (if (equal? compare-e e)
                             (set! to-insert (cons (list #f e) to-insert))
                             (set! to-insert (cons (list #t e) to-insert))))
                       compare-v v)
             (set! to-insert (reverse to-insert))
             (for-each (lambda (p)
                         (cond 
                           [(first p)
                            (define start-pos (last-position))
                            (insert (format "~a = ~v\n" (car (second p)) (cdr (second p))))
                            (define end-pos (sub1 (last-position)))
                            (change-style changed-style start-pos end-pos)]
                           [else
                            (insert (format "~a = ~v\n" (car (second p)) (cdr (second p))))]))
                       to-insert)]
            [else (insert (data-list->string v))])
          (end-edit-sequence)
          (lock #t))
        (initialize)))
    
    (define/public (initialize-series s) (set! series s))
    
    (define/override (adjust-cursor dc x y editorx editory event)
      (make-object cursor% 'arrow))
    
    (define/private (initialize-scrub-frame)
      (set! scrub-frame (new frame%
                             [label "Scrub View"]
                             [width frame-width]
                             [height frame-height]))
      (set! main-panel (new vertical-panel% [parent scrub-frame]))
      (set! scrub-text (new scrub-text%))
      (new editor-canvas%
           [parent main-panel]
           [editor scrub-text])
      (set! scrub-slider (new scrub-slider% 
                              [text scrub-text]
                              [step (sub1 (length series))] 
                              [frame-width frame-width]
                              [min-height 42]
                              [stretchable-height #f]
                              [parent main-panel])))
   
    (define/override (on-event dc x y editorx editory event)
      (when (send event button-down? 'left)
        (unless scrub-frame (initialize-scrub-frame))
        (unless (send scrub-frame is-shown?)
          (send scrub-frame show #t)))
      (super on-event dc x y editorx editory event))
    
    (super-new)))

(define aggregate-editor%
  (class text%
    (init-field [data null])
    (inherit insert lock
             begin-edit-sequence
             end-edit-sequence)
    
    (define/private (display-aggregate-traces)
      (begin-edit-sequence)
      (for-each
       (lambda (l)
         (unless (= (length l) 1)
           (define scrub (make-object scrub-snip% scrub-icon))
           (send scrub initialize-series l)
           (send scrub set-flags (list 'handles-all-mouse-events))
           (insert scrub)
           (insert " "))
         (for-each (lambda (s)
                     (let* ([text (new text%)]
                            [snip (new aggregate-snip% [editor text])])
                       (send text insert (data-list->string s))
                       (send snip show-border #f)
                       (insert snip)))
                   l)
         (insert "\n\n"))
       data)
      (end-edit-sequence)
      (lock #t))
    
    (super-new)
    (display-aggregate-traces)))
    
    


