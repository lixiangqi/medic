#lang scribble/doc

@(require scribble/manual
          redex/pict
          scriblib/figure
          (for-label (except-in racket log export import remove and)
                     racket/contract/base
                     medic/trace
                     (only-in medic/main 
                              layer export import def in with-behavior ref each-function
                              on-entry on-exit at)))

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
  #:literals (layer export import def in with-behavior ref each-function
              on-entry on-exit at)
  [top-level-form (layer-form layer-form ...)]        
  [layer-form (layer layer-id layer-expr ...)
              (layer layer-id #:enable flag layer-expr ...)]
  [layer-expr (export id id ...)
              (import layer-id layer-id ...)
              debug-expr]
  [debug-expr (def debug-src-id #:src source-expr source-expr ...)
              (def debug-id #:debug match-expr match-expr ...)
              (in #:module module-path match-expr match-expr ...)]
  [match-expr (with-behavior f template)
              (with-behavior f template [renamed ret id])
              (ref debug-id)
              insert-expr
              [each-function insert-expr insert-expr ...]
              [(f f ...) insert-expr insert-expr ...]]
  [insert-expr border-expr
               at-expr]
  [border-expr [on-entry source-expr source-expr ...]
               [on-exit source-expr source-expr ...]]
  [at-expr [(at location-expr) border-expr border-expr ...]
           [(at location-expr before-expr) border-expr border-expr ...]
           [(at location-expr after-expr) border-expr border-expr ...]
           [(at location-expr before-expr after-expr) border-expr border-expr ...]]
  [location-expr target-language-expression
                 expression-pattern]
  [before-expr [#:before location-expr location-expr ...]]
  [after-expr [#:after location-expr location-expr ...]]
  [source-expr (ref debug-src-id)
               target-language-expression]
  [flag boolean]
  [f variable-not-otherwise-mentioned]
  [id variable-not-otherwise-mentioned]
  [layer-id variable-not-otherwise-mentioned]
  [debug-src-id variable-not-otherwise-mentioned]
  [debug-id variable-not-otherwise-mentioned])

@defform*[((layer layer-id layer-expr ...)
           (layer layer-id #:enable flag layer-expr ...))]{
Modularizes debugging code and facilitates organizing debugging traces into different units. 
The @racket[#:enable] keyword permits enabling and disabling adding to the source code
the debugging behaviors described within @racketvarfont{layer-expr}, while the debugging definitions
within the layer are still available to other layers.  
}

@defform[(export id id ...)]{
Declares exports of a layer where the @racketvarfont{id} is the identifier of an internal
layer definition.}

@defform[(import layer-id layer-id ...)]{
Declares imports of a layer where the @racketvarfont{layer-id} is some layer identifier.}

@defform*[((def debug-src-id #:src source-expr source-expr ...)
           (def debug-id #:debug match-expr match-expr ...))]{
Binds @racketvarfont{debug-src-id} to a sequence of source expressions following the
@racket[#:src] keyword and @racketvarfont{debug-id} to a sequence of medic expressions
following the @racket[#:debug] keyword.}

@defform*[((ref debug-src-id)
           (ref debug-id))]{
Accesses the corresponding source expressions or medic expressions bound by 
@racketvarfont{debug-src-id} or @racketvarfont{debug-id}.
}

@defform[(in #:module module-path match-expr match-expr ...)]{
Specifies the module to apply debugging code. The @racketvarfont{module-path} 
can be three kinds of paths: a relative path, an absolute path, or a library path. 
For example, the following are possible specifications for @racketvarfont{module-path}.
@racketblock[
(code:comment "a relative path")
(in #:module "src.rkt" ....)
(code:comment "an absolute path")
(in #:module (file "/home/xiangqi/test/src.rkt" ....))
(code:comment "a library path")
(in #:module test/src ....)
]}

@defform*[((with-behavior f template)
           (with-behavior f template [renamed ret id]))]{
Defines the logging behavior of the @racketvarfont{f} function call. The form of @racketvarfont{template}
is @litchar["@"]@litchar["{"]@racketvarfont{text-body}@litchar["}"]. In @racketvarfont{text-body},
@litchar["@"]@racket[expr] enables the evaluation of an expression @racket[expr] with access to functions'
arguments and return value. A function's arguments can be accessed by @racket[par] where @racket[par] is the 
parameter's name. By default, @racket[ret] reveals the return value of a function. To avoid 
confusions of referring to some argument with @racket[ret] name rather than the default return value, 
use @racket[[renamed ret id]] explicitly to rename the symbol of the return value. For example,

@codeblock{
(with-behavior f @"@"@"{"f takes @"@"x and @"@"y and returns @"@"ret@"}")
(with-behavior f @"@"@"{"f takes @"@"x and @"@"ret and returns @"@"ret1@"}" [renamed ret ret1])
}

See @secref["log"] for more information.}


@defform[[each-function insert-expr insert-expr ...]]{
Adds a sequence of @racket[insert-expr] to every function defined in the module.}

@defform/none[[(f f ...) insert-expr insert-expr ...]]{
Supports function-level matching where @tt{(@racketvarfont{f f} ...)} constrains the scope of 
a sequence of @racket[insert-expr] to be within one or more functions. The fourth
clause of the @racketvarfont{match-expr} non-terminal is module-level matching. }

@defform*[#:id at 
          ([(at location-expr) border-expr border-expr ...]
           [(at location-expr before-expr) border-expr border-expr ...]
           [(at location-expr after-expr) border-expr border-expr ...]
           [(at location-expr before-expr after-expr) border-expr border-expr ...])]{
Supports expression-level matching of the target expression @racket[location-expr]. For Racket, @racket[location-expr]
can be any expression in the source program except internal definitions, such as the @racket[(define ....)] form inside a
function or the @racket[(let ....)] local binding form.  To avoid the confusions of multiple matches of
@racketvarfont{location-expr} in the target program, specification of @racketvarfont{before-expr} 
and @racketvarfont{after-expr} can be employed to confine the lexical context of @racketvarfont{location-expr}.
Expressions within @racketvarfont{location-expr}, @racketvarfont{before-expr} and @racketvarfont{after-expr} can be a legal
source expression or an expression pattern with @litchar["_"] wildcard character matching any legal expression. For 
example,

@codeblock{
[(at (+ _ 1) [#:before (define x (inc 4))]) [on-entry (log x)]]
}
}

@defform[[on-entry source-expr source-expr ...]]{
Inserts a sequence of debugging code @racket[source-expr] before the target expression located by @racketvarfont{at-expr},
or at the beginning of a function or module depending on the scope of matching.}
@defform[[on-exit source-expr source-expr ...]]{
Inserts a sequence of debugging code @racket[source-expr] after the target expression located by @racketvarfont{at-expr},
or at the end of a function or module depending on the scope of matching.}
