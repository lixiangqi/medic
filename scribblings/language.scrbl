#lang scribble/doc

@(require scribble/manual
          redex/pict
          scriblib/figure
          (for-label (except-in racket log export import remove and)
                     racket/contract/base
                     medic/trace
                     (only-in medic/main 
                              layer export import define-source define-match in with-behavior each-function
                              on-entry on-exit at function-name)))

@title{A Metaprogramming Language}
@defmodulelang[medic]
The Medic debugger treats debugging as a metaprogramming activity. Programmers write a debugging
program to augment the source program with desirable debugging behaviors without changing the source
program. The separation of a debugging program from a source program enables reusing and programming
of the debugging interaction as well as keeping the source program intact. The debugging program can 
serve as a form of documentation, which preserves
the efforts invested in debugging, and act as something akin to testing suites that run against
a modified program later during the process of software development.

Here is the grammar for the Medic metaprogramming language:

@(racketgrammar* 
  #:literals (layer export import define-source define-match
              in with-behavior each-function
              on-entry on-exit at)
  [top-level-form layer-def...]        
  [layer-def (layer layer-id layer-form ...)
             (layer layer-id #:enable flag layer-form ...)]
  [layer-form (export id ...)
              (import layer-id ...)
              debug-def
              (in #:module module-name match-form ...)]
  [debug-def (define-source debug-src-id source-expr)
             (define-source (debug-src-id arg-id ...) source-expr ...)
             (define-match debug-id match-form)
             (define-match (debug-id arg-id ...) match-form ...)]
  [match-form (with-behavior f template)
              (with-behavior f template #:renamed ret id)
              match-ref-form
              insert-form
              [each-function insert-form ...]
              [(f ...) insert-form ...]]
  [insert-form border-form
               at-form]
  [border-form [on-entry source-expr ...]
               [on-exit source-expr ...]]
  [at-form [at location-form border-form ...]
           [at location-form #:before location-form border-form ...]
           [at location-form #:after location-form border-form ...]
           [at location-form #:before location-form #:after location-form border-form ...]]
  [location-form target-language-expression
                 expression-pattern]
  [source-expr src-ref-form
               target-language-expression]
  [match-ref-form debug-id
                  (debug-id match-arg ...)]
  [src-ref-form debug-src-id
                (debug-src-id src-arg ...)]
  [flag boolean]
  [f variable-not-otherwise-mentioned]
  [id variable-not-otherwise-mentioned]
  [layer-id variable-not-otherwise-mentioned]
  [(debug-src-id arg-id) variable-not-otherwise-mentioned]
  [debug-id variable-not-otherwise-mentioned])

@defform*[((layer layer-id layer-form ...)
           (layer layer-id #:enable flag layer-form ...))]{
Modularizes debugging code and facilitates organizing debugging traces into different units. 
The @racket[#:enable] keyword permits enabling and disabling adding to the source code
the debugging behaviors described within @racketvarfont{layer-form}, while the debugging definitions
within the layer are still available to other layers.  
}

@defform[(export id ...)]{
Declares exports of a layer where the @racketvarfont{id} is the identifier of an internal
layer definition.}

@defform[(import layer-id ...)]{
Declares imports of a layer where the @racketvarfont{layer-id} is some layer identifier.}

@defform*[((define-source debug-src-id source-expr)
           (define-source (debug-src-id arg-id ...) source-expr ...))]{
Binds @racketvarfont{debug-src-id} to a sequence of source expressions.}

@defform*[((define-match debug-id match-form)
           (define-match (debug-id arg-id ...) match-form ...))]{
Binds @racketvarfont{debug-id} to a sequence of match forms.}

@defform[(in #:module module-name match-expr ...)]{
Specifies the module to apply debugging code. The @racketvarfont{module-name} 
can be three kinds of paths: a relative path, an absolute path, or a library path. 
For example, the following are possible specifications for @racketvarfont{module-name}.
@racketblock[
(code:comment "a relative path")
(in #:module "src.rkt" ....)
(code:comment "an absolute path")
(in #:module (file "/home/xiangqi/test/src.rkt" ....))
(code:comment "a library path")
(in #:module test/src ....)
]}

@defform*[((with-behavior f template)
           (with-behavior f template #:renamed ret id))]{
Defines the logging behavior of the @racketvarfont{f} function call. The form of @racketvarfont{template}
is @litchar["@"]@litchar["{"]@racketvarfont{text-body}@litchar["}"]. In @racketvarfont{text-body},
@litchar["@"]@racket[expr] enables the evaluation of an expression @racket[expr] with access to functions'
arguments and return value. A function's arguments can be accessed by @racket[par] where @racket[par] is the 
parameter's name. By default, @racket[ret] reveals the return value of a function. To avoid 
confusions of referring to some argument with @racket[ret] name rather than the default return value, 
use @racket[#:renamed ret id] explicitly to rename the symbol of the return value. For example,

@codeblock{
(with-behavior f @"@"@"{"f takes @"@"x and @"@"y and returns @"@"ret@"}")
(with-behavior f @"@"@"{"f takes @"@"x and @"@"ret and returns @"@"ret1@"}" #:renamed ret ret1)
}

See @secref["log"] for more information.}

@defthing[function-name (or/c #f string?)]{
Returns the function name for the current scope of evaluation of @litchar["@"]@racket[funtion-name]. The 
debugging primitive @litchar["@"]@racket[funtion-name] exposes the run-time function scope, which is only 
available to debuggers, to programmers.
}
@defform[[each-function insert-form ...]]{
Adds a sequence of @racket[insert-form]s to every function defined in the module.}

@defform/none[[(f ...) insert-form ...]]{
Supports function-level matching where @tt{(@racketvarfont{f} ...)} constrains the scope of 
a sequence of @racket[insert-form] to be within one or more functions. The fourth
clause of the @racketvarfont{match-form} non-terminal is module-level matching. }

@defform*[#:id at 
          ([at location-form border-form ...]
           [at location-form #:before location-form border-form ...]
           [at location-form #:after location-form border-form ...]
           [at location-form #:before location-form #:after location-form border-form ...])]{
Supports expression-level matching of the target expression @racket[location-form]. For Racket, @racket[location-form]
can be any expression in the source program except internal definitions, such as the @racket[(define ....)] form inside a
function or the @racket[(let ....)] local binding form.  To avoid the confusions of multiple matches of
@racketvarfont{location-form} in the target program, specification of @racket[#:before]
and @racket[#:after] can be employed to confine the lexical context of @racketvarfont{location-form}.
Expressions within @racketvarfont{location-form}, or after @racket[#:before] and @racket[#:after] can be a legal
source expression or an expression pattern with @litchar["_"] wildcard character matching any legal expression. For 
example,

@codeblock{
[at (+ _ 1) #:before (define x (inc 4)) [on-entry (log x)]]
}
}

@defform[[on-entry source-expr ...]]{
Inserts a sequence of debugging code @racket[source-expr]s before the target expression located by @racketvarfont{at-form},
or at the beginning of a function or module depending on the scope of matching.}
@defform[[on-exit source-expr ...]]{
Inserts a sequence of debugging code @racket[source-expr] after the target expression located by @racketvarfont{at-form},
or at the end of a function or module depending on the scope of matching.}
