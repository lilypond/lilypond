%%%% guile-debugger.ly
%%%%
%%%% Source file of the GNU LilyPond music typesetter
%%%%
%%%% (c) 2009 by Ian Hulin <ian@hulin.org.uk>

%%  \include this file to enable the setting of breakpoints in guile.
%%  Once loaded, this file will open a guile debug prompt.  Type
%%  help
%%  at the debug prompt to get a list of possible commands.
%%  For more information, see the Contributors' Guide.


\version "2.13.4"

#(use-modules
  (ice-9 debugger)
  (ice-9 debugging trace)
  (ice-9 debugging steps)
  (ice-9 debugging ice-9-debugger-extensions))

#(define (break! proc)
   (install-trap (make <procedure-trap>
                   #:procedure proc
                   #:behaviour debug-trap)))

#(define (trace! proc)
   (install-trap (make <procedure-trap>
                   #:procedure proc
                   #:behaviour (list trace-trap
                                     trace-at-exit))))

#(define (trace-subtree! proc)
   (install-trap (make <procedure-trap>
                   #:procedure proc
                   #:behaviour (list trace-trap
                                     trace-until-exit))))

#(module-define! (resolve-module '(guile-user))
                 'lilypond-module
                 (current-module))
#(top-repl)
#(set-current-module lilypond-module)
