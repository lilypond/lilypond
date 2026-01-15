%%%% Scheme sandbox.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2011--2023 David Kastrup <dak@gnu.org>
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

\version "2.24.0"

% Set a default prompt.
#(use-modules (system repl common))
#(repl-default-option-set! 'prompt "guile>")

% The next command loads the user's init file (usually `~/.guile`) for
% interactive sessions.
#(load-user-init)

% Try to activate 'readline' support, which greatly enhances the
% functionality of the guile prompt.
#(cond
  ((with-exception-handler (lambda (exception) #f)
     (lambda ()
       (use-modules (ice-9 readline)))
     #:unwind? #t)
   (activate-readline))
  (else
   (ly:warning "

  Cannot load dynamic library 'guile-readline'.  Either you are using
  a statically linked LilyPond binary (for example, the binary that
  comes with the official distribution), or you haven't installed the
  'guile-readline' and 'libreadline' packages for your system.

  Possible workarounds:

  . Use the 'rlwrap' program.
  . Use the Emacs editor with the 'lilypond-ts-mode' package.
")))

#(newline)
#(begin
  (use-modules (system repl repl))
  (start-repl))
