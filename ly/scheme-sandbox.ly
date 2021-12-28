%%%% Scheme sandbox.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2011--2022 David Kastrup <dak@gnu.org>
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

\version "2.16.0"

#(load-user-init)

% This loads the user's .guile file for interactive sessions.
% One typical thing you might want to put there is
% (use-modules (ice-9 readline))
% (activate-readline)
% in order to activate command line editing for interactive sessions.
% You need libreadline support and the respective Guile module to be
% installed for that.  In Debian, for example, this is part of the
% guile-1.8-libs package.  Depending on your system and version, the
% requirements may be different.

#(newline)
#(cond-expand
   (guile-2
     (begin
       (use-modules (system repl repl))
       (start-repl)))
   (else (scm-style-repl)))
