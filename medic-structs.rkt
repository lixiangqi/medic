#lang racket

(provide (all-defined-out))

; env structure
; exports, imports: list of identifiers
; import-table: list of pair (layer-id, list-of-exported-ids)
; src-table: map from identifier to source expressions (all expanded, no ref form inside)
; debug-table: map from identifier to debugging matching expressions (may contain ref form, lazy evaluation)
(struct env (exports imports import-table src-table debug-table) #:transparent)

; at-insert structure
; at-expr: the current at epxression
; scope: list of function identifier (string) or ‘module
; target: the target expression to be located (string)
; before: the expressions before the target expression (list of string)
; after: the expressions after the target expression (list of string)
; loc: ‘exit or ‘entry
; exprs: expressions to be inserted
(struct at-insert (at-expr scope target before after loc exprs) #:transparent)

; finer-at-insert structure
; constructed on top of at-insert structure, where posns contains the found expression positions in editor
(struct finer-at-insert (at-expr [scope #:mutable] target [posns #:mutable] loc exprs) #:transparent)

(struct import-struct (layer-id exported) #:transparent)

(struct insert-struct (stx loc exprs) #:transparent)