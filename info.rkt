#lang info

(define scribblings
  '(("scribblings/medic.scrbl" (multi-page))))

(define deps '("at-exp-lib"
               "base" "scheme-lib" "compatibility-lib" "gui-lib" "images-lib"
               "pict-lib" "unstable-lib" "draw-lib"))
(define build-deps '("racket-doc" "scribble-lib" "redex-pict-lib"))
