%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2009--2022 by Ian Hulin <ian@hulin.org.uk>
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
%%  Once loaded, this file will open a guile prompt.  Type
%%  (debug-help)
%%  at the guile prompt to get a list of possible commands.
%%  For more information, see the Contributor's Guide.


\version "2.23.2"

% define lilypond-module as a variable in the guile-user module and set
% to the current Scheme module (which will be the lilypond top-level
% module)

#(module-define! (resolve-module '(guile-user))
                 'lilypond-module
                 (current-module))
%
% Ensure we have command-line recall available at the guile prompt
%
#(use-modules (ice-9 readline))
#(activate-readline)
#(display "\n Guile debugger for Lilypond")
#(display "\n For help enter (debug-help)\n")
%
% Ensure debugger definitions are available in lilypond-module and guile-user
%
#(use-modules (lily guile-debugger))
#(ly:module-copy (resolve-module '(guile-user))
                 (resolve-module '(lily guile-debugger)))
#(top-repl)
%
% top-repl has re-set the module to guile-user,
%  so we need to set it back to lilypond-module
%
#(ly:module-copy (current-module) (resolve-module '(lilypond-module)))
#(set-current-module lilypond-module)
