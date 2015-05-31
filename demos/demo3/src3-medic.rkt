#lang medic

(layer layer1 
       (in #:module "src3.rkt"
           ; scope of multiple functions 
           [(g inc) [on-entry @log{function @function-name : x = @x}]]
           ; each-function primitive
           [each-function [on-entry @log{function @function-name entered}]]))

