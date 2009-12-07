%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2009 by Ian Hulin <ian@hulin.org.uk>
%%%%
%%%% LilyPond is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%%
%%%% LilyPond is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%%
%%%% You should have received a copy of the GNU General Public License
%%%% along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.



%%  \include this file to enable the setting of breakpoints in guile.
%%  Once loaded, this file will open a guile debug prompt.  Type
%%  help
%%  at the debug prompt to get a list of possible commands.
%%  For more information, see the Contributor's Guide.


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
